


rbernoulli <- function(n, p = 0.5) stats::runif(n) > (1 - p)

calculate_transaction_cbs_data <- function(tnx_data_tbl, last_date) {
  cbs_data_tbl <- tnx_data_tbl |>
    filter(tnx_timestamp <= last_date) |>
    group_by(customer_id) |>
    summarise(
      .groups = "drop",

      first_tnx_date = min(tnx_timestamp),
      last_tnx_date  = max(tnx_timestamp),

      x     = n() - 1,
      t_x   = difftime(last_tnx_date, first_tnx_date, units = "weeks") |> as.numeric(),
      T_cal = difftime(last_date,     first_tnx_date, units = "weeks") |> as.numeric()
      )

  return(cbs_data_tbl)
}



generate_customer_cohort_data <- function(n_customers, first_date, last_date) {
  tnx_dates <- seq(first_date, last_date, by = "day")

  customer_cohort_tbl <- tnx_dates |>
    enframe(name = NULL, value = "first_tnx_date") |>
    slice_sample(n = n_customers, replace = TRUE) |>
    arrange(first_tnx_date) |>
    group_by(format(first_tnx_date, "%Y%m")) |>
    mutate(
      customer_id = sprintf("C%s_%04d", format(first_tnx_date, "%Y%m"), 1:n()),
      cohort_qtr  = first_tnx_date |> as.yearqtr() |> as.character(),
      cohort_ym   = first_tnx_date |> format("%Y %m")
      ) |>
    ungroup() |>
    select(customer_id, cohort_qtr, cohort_ym, first_tnx_date)

  return(customer_cohort_tbl)
}


calculate_event_times <- function(rate, total_time, block_size = 100) {
  sample_vec <- c()

  sample_complete <- FALSE

  while(!sample_complete) {
    block_sample <- rexp(block_size, rate = rate)

    sample_vec <- c(sample_vec, block_sample)

    cuml_value <- cumsum(sample_vec)

    sample_complete <- any(cuml_value > total_time)
  }

  event_times <- sample_vec[cumsum(sample_vec) < total_time]

  return(event_times)
}


generate_pnbd_customer_simulation_params <- function(customer_cohort_data_tbl,
                                                     params_lst) {



  lambda_mn  <- params_lst$lambda_mn
  lambda_cv  <- params_lst$lambda_cv
  mu_mn      <- params_lst$mu_mn
  mu_cv      <- params_lst$mu_cv
  amt_hiermn <- params_lst$amt_hiermn
  amt_hiercv <- params_lst$amt_hiercv
  amt_custcv <- params_lst$amt_custcv

  customer_basic_parameters_tbl <- customer_cohort_data_tbl |>
    select(customer_id, cohort_qtr, cohort_ym, first_tnx_date) |>
    arrange(first_tnx_date, customer_id) |>
    mutate(
      customer_lambda = rgamma_mucv(n(), mu = lambda_mn,  cv = lambda_cv),
      customer_mu     = rgamma_mucv(n(), mu = mu_mn,      cv = mu_cv),
      customer_tau    = rexp(n(), rate = customer_mu),
      customer_amtmn  = rgamma_mucv(n(), mu = amt_hiermn, cv = amt_hiercv),
      customer_amtcv  = amt_custcv
      )

  return(customer_basic_parameters_tbl)
}


generate_pnbd_customer_transaction_data <- function(sim_params_tbl, final_tnx_date) {

  if(final_tnx_date < sim_params_tbl |> pull(first_tnx_date) |> max()) {
    warning(glue(
      "Customer first_tnx_date is greater than the value of final_tnx_date: {final_tnx_date} - returning value FALSE"
      ))

    return(FALSE)
  }

  customer_transactions_tbl <- sim_params_tbl |>
    mutate(
      sim_data = pmap(
        list(
          lifetime    = customer_tau,
          tnx_rate    = customer_lambda,
          amt_mn      = customer_amtmn,
          amt_cv      = customer_amtcv,
          first_date  = first_tnx_date,
          customer_id = customer_id
          ),
        generate_pnbd_individual_transactions,
        final_date = final_tnx_date
        )
      ) |>
    select(
      customer_id, cohort_qtr, cohort_ym, sim_data
      ) |>
    unnest(sim_data)

  return(customer_transactions_tbl)
}


generate_pnbd_individual_transactions <- function(
    lifetime, tnx_rate, amt_mn, amt_cv, first_date,
    customer_id = customer_id, final_date = final_date) {

  obs_weeks <- difftime(final_date, first_date, units = "weeks") |> as.numeric()

  tnx_window <- min(obs_weeks, lifetime)

  if(tnx_window < 0) warning("Invalid tnx_window value: should not be negative")

  first_tnx_dttm <- as.POSIXct(first_date) + runif(1, min = 0, max = 24 * 60 * 60 - 1)

  tnx_intervals <- calculate_event_times(
    rate       = tnx_rate,
    total_time = tnx_window,
    block_size = 1000
    )

  event_dates <- first_tnx_dttm + (cumsum(tnx_intervals) * (7 * 24 * 60 * 60))

  tnx_amounts <- rgamma_mucv(1 + length(event_dates), mu = amt_mn, cv = amt_cv)

  sim_tnx_tbl <- tibble(
    tnx_timestamp = c(first_tnx_dttm, event_dates),
    tnx_amount    = tnx_amounts |> round(2)
    )

  return(sim_tnx_tbl)
}


generate_transaction_metadata <- function(data_tbl) {
  transactions_tbl <- data_tbl |>
    arrange(tnx_timestamp) |>
    group_by(tnx_date = as.Date(tnx_timestamp)) |>
    mutate(
      invoice_id = sprintf("T%s-%04d", format(tnx_date, "%Y%m%d"), 1:n())
      ) |>
    ungroup() |>
    select(customer_id, tnx_timestamp, invoice_id, tnx_amount)

  return(transactions_tbl)
}


create_pnbd_posterior_validation_data <- function(stanfit, data_tbl, simparams_tbl, bincount = 50) {

  validation_tbl <- stanfit |>
    recover_types(data_tbl) |>
    spread_draws(lambda[customer_id], mu[customer_id], p_alive[customer_id]) |>
    ungroup() |>
    inner_join(simparams_tbl, by = "customer_id") |>
    select(
      customer_id, draw_id = .draw, post_lambda = lambda, customer_lambda,
      post_mu = mu, customer_mu, p_alive
      )

  tmp_tbl <- validation_tbl |>
    calculate_distribution_qvals(post_lambda, customer_lambda, customer_id)

  qvalues_tbl <- validation_tbl |>
    calculate_distribution_qvals(post_mu, customer_mu, customer_id) |>
    rename(qval_mu = q_val) |>
    inner_join(tmp_tbl, by = "customer_id") |>
    select(
      customer_id, customer_lambda, qval_lambda = q_val, customer_mu, qval_mu
    )


  unif_count <- qvalues_tbl |>
    nrow() |>
    divide_by(bincount)

  lambda_qval_plot <- ggplot(qvalues_tbl) +
    geom_histogram(aes(x = qval_lambda), bins = bincount) +
    geom_hline(aes(yintercept = unif_count), colour = "red") +
    labs(
      x = "q-Values",
      y = "Frequency",

      title = "Histogram of the q-Values for Lambda"
      )

  mu_qval_plot <- ggplot(qvalues_tbl) +
    geom_histogram(aes(x = qval_mu), bins = bincount) +
    geom_hline(aes(yintercept = unif_count), colour = "red") +
    labs(
      x = "q-Values",
      y = "Frequency",

      title = "Histogram of the q-Values for Mu"
      )


  valid_lst <- list(
    validation_tbl   = validation_tbl,
    qvalues_tbl      = qvalues_tbl,

    mu_qval_plot     = mu_qval_plot,
    lambda_qval_plot = lambda_qval_plot
    )

  return(valid_lst)
}


run_pnbd_simulations_chunk <- function(
    sim_file, param_tbl, start_dttm, end_dttm
    ) {

  calc_file <- !file_exists(sim_file)

  if(calc_file) {
    simdata_tbl <- param_tbl |>
      mutate(
        sim_data = pmap(
          list(
            p_alive  = p_alive,
            lambda   = post_lambda,
            mu       = post_mu
            ),
          generate_pnbd_validation_transactions,

          tnx_mu     = 1,
          tnx_cv     = 1,
          start_dttm = start_dttm,
          end_dttm   = end_dttm
          ),
        sim_tnx_count = map_int(sim_data, nrow),
        max_data = map(
          sim_data,
          ~ .x |>
            slice_max(n = 1, order_by = tnx_timestamp, with_ties = FALSE) |>
            select(sim_tnx_last = tnx_timestamp)
          )
        ) |>
      unnest(max_data, keep_empty = TRUE)

    simdata_tbl |> write_rds(sim_file)
  }


  return(calc_file)
}


generate_pnbd_validation_transactions <- function(p_alive, lambda, mu, tnx_mu, tnx_cv, start_dttm, end_dttm) {

  customer_active <- rbernoulli(n = 1, p = p_alive)

  max_obs <- difftime(end_dttm, start_dttm, units = "weeks")


  if(customer_active) {
    tau <- rexp(n = 1, rate = mu)

    obs_time <- min(tau, max_obs)

    tnx_intervals <- calculate_event_times(
      rate       = lambda,
      total_time = obs_time,
      block_size = 1000
      )

    event_dates <- start_dttm + (cumsum(tnx_intervals) * (7 * 24 * 60 * 60))

    tnx_amounts <- rgamma_mucv(length(event_dates), mu = tnx_mu, cv = tnx_cv)

    tnxdata_tbl <- tibble(
        tnx_timestamp = event_dates,
        tnx_amount    = tnx_amounts %>% round(2)
        ) |>
      filter(
        tnx_timestamp <= end_dttm
      )

  } else {
    tnxdata_tbl <- tibble(
        tnx_timestamp = start_dttm,
        tnx_amount    = 0
        ) |>
      slice(0)
  }


  return(tnxdata_tbl)
}

