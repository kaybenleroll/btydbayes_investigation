
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



generate_customer_cohort_data <- function(n_customers, first_date, last_date, id_prefix = "C") {
  tnx_dates <- seq(first_date, last_date - 1, by = "day")

  customer_cohort_tbl <- tnx_dates |>
    enframe(name = NULL, value = "first_tnx_date") |>
    slice_sample(n = n_customers, replace = TRUE) |>
    arrange(first_tnx_date) |>
    group_by(format(first_tnx_date, "%Y%m")) |>
    mutate(
      customer_id = sprintf("%s%s_%04d", id_prefix, format(first_tnx_date, "%Y%m"), 1:n()),
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
    unnest(sim_data) |>
    filter(tnx_timestamp < final_tnx_date)


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


construct_pnbd_posterior_statistics <- function(stanfit, fitdata_tbl) {
  post_stats_tbl <- stanfit |>
    recover_types(fitdata_tbl) |>
    spread_draws(lambda[customer_id], mu[customer_id], p_alive[customer_id]) |>
    ungroup() |>
    inner_join(fitdata_tbl, by = "customer_id") |>
    select(
      customer_id, first_tnx_date, draw_id = .draw,
      post_lambda = lambda, post_mu = mu, p_alive
    )

  return(post_stats_tbl)
}


run_simulations_chunk <- function(sim_param_tbl, sim_file, sim_func) {

  simdata_tbl <- sim_param_tbl |>
    group_nest(draw_id, .key = "sim_params") |>
    mutate(
      sim_data      = map(sim_params, sim_func),
      sim_tnx_count = map_int(sim_data, nrow),
      last_data = map(
        sim_data,
        ~ .x |>
          slice_max(n = 1, order_by = tnx_timestamp, with_ties = FALSE) |>
          select(sim_tnx_last = tnx_timestamp)
        )
      ) |>
    unnest(last_data, keep_empty = TRUE) |>
    unnest(sim_params)

  simdata_tbl |> write_rds(sim_file)

  return(simdata_tbl |> nrow())
}


generate_pnbd_validation_transactions <- function(sim_params_tbl) {

  start_dttm <- sim_params_tbl$start_dttm
  end_dttm   <- sim_params_tbl$end_dttm
  p_alive    <- sim_params_tbl$p_alive
  lambda     <- sim_params_tbl$lambda
  mu         <- sim_params_tbl$mu
  tnx_mu     <- sim_params_tbl$tnx_mu
  tnx_cv     <- sim_params_tbl$tnx_cv


  extra_tau    <- rexp(n = 1, rate = mu)
  max_observed <- difftime(end_dttm, start_dttm, units = "weeks")

  customer_active <- stats::runif(1) > (1 - p_alive)


  if(customer_active) {
    obs_time <- min(extra_tau, max_observed)

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


run_model_assessment <- function(
    model_stanfit, insample_tbl, outsample_tbl, fit_label,
    fit_end_dttm, valid_start_dttm, valid_end_dttm,
    precompute_rootdir = "precompute", data_dir = "data",
    sim_seed = 420) {


  ###
  ### Ensuring the precompute_dir folder exists
  ###
  precompute_dir <- glue("{precompute_rootdir}/{fit_label}")

  syslog(
    glue("Ensuring precompute directory {precompute_dir} exists"),
    level = "INFO"
    )

  ensure_exists_precompute_directory(precompute_dir)


  ###
  ### Setting up the simulation statistics
  ###
  syslog(
    glue("Calculating the posterior statistics"),
    level = "INFO"
    )

  model_simstats_filepath <- glue("{data_dir}/{fit_label}_assess_model_simstats_tbl.rds")

  if(!file_exists(model_simstats_filepath)) {
    model_simstats_tbl <- construct_pnbd_posterior_statistics(
      stanfit     = model_stanfit,
      fitdata_tbl = insample_tbl
    )

    model_simstats_tbl |> write_rds(model_simstats_filepath, compress = "gz")
  } else {

    model_simstats_tbl <- read_rds(model_simstats_filepath)
  }





  ### Setting up the sim_stats function
  retrieve_sim_stats <- ~ .x |>
    read_rds() |>
    select(draw_id, sim_data, sim_tnx_count, sim_tnx_last)

  ### We also want to get the contents of the precompute_dir directory
  precomputed_tbl <- dir_ls(glue("{precompute_dir}")) |>
    as.character() |>
    enframe(name = NULL, value = "sim_file")



  ###
  ### Setting up and calculating the in-sample simulations
  ###
  syslog(
    glue("Setting up insample model_index_tbl"),
    level = "INFO"
    )

  model_index_tbl <- model_simstats_tbl |>
    mutate(
      start_dttm = first_tnx_date,
      end_dttm   = fit_end_dttm,
      lambda     = post_lambda,
      mu         = post_mu,
      p_alive    = 1,      ### In-sample validation, so customer begins active
      tnx_mu     = 100,    ### We are not simulating tnx size, so put in defaults
      tnx_cv     = 1       ###
      ) |>
    group_nest(customer_id, .key = "cust_params", keep = TRUE) |>
    mutate(
      sim_file = glue(
        "{precompute_dir}/sims_fit_{fit_label}_{customer_id}.rds"
        )
      )

  syslog(
    glue("Configuring the insample fit simulations"),
    level = "INFO"
    )

  runsims_tbl <- model_index_tbl |>
    anti_join(precomputed_tbl, by = "sim_file")

  n_sims <- runsims_tbl |> nrow()

  if(n_sims > 0) {

    syslog(
      glue("Running {n_sims} in-sample simulations to {precompute_dir}"),
      level = "INFO"
      )

    plan(multisession)

    model_index_tbl <- runsims_tbl |>
      mutate(
        chunk_data = future_map2_int(
          cust_params, sim_file,
          run_simulations_chunk,

          sim_func = generate_pnbd_validation_transactions,

          .options = furrr_options(
            globals  = c(
              "calculate_event_times", "rgamma_mucv", "gamma_mucv2shaperate",
              "generate_pnbd_validation_transactions"
              ),
            packages   = c("tidyverse", "fs"),
            scheduling = Inf,
            seed       = sim_seed + 1
            ),

          .progress = TRUE
          )
        )
  }


  syslog(
    glue("Setting up model_fit_simstats_tbl"),
    level = "INFO"
    )

  model_fit_simstats_filepath <- glue("{data_dir}/{fit_label}_assess_fit_simstats_tbl.rds")

  if(!file_exists(model_fit_simstats_filepath)) {
    model_simdata_tbl <- model_index_tbl |>
      mutate(
        sim_data = map(
          sim_file, retrieve_sim_stats,

          .progress = "retrieve_fit_stats"
          )
        ) |>
      select(customer_id, sim_data) |>
      unnest(sim_data)

    model_simdata_tbl |> write_rds(model_fit_simstats_filepath, compress = "gz")

    rm(model_simdata_tbl)
  }


  ###
  ### Setting up and calculating the out-of-sample simulations
  ###

  syslog(
    glue("Setting up out-of-sample validation model_index_tbl"),
    level = "INFO"
    )

  model_index_tbl <- model_simstats_tbl |>
    mutate(
      start_dttm = valid_start_dttm,
      end_dttm   = valid_end_dttm,
      lambda     = post_lambda,
      mu         = post_mu,
      tnx_mu     = 1,      ### We are not simulating tnx size
      tnx_cv     = 1       ###
      ) |>
    group_nest(customer_id, .key = "cust_params") |>
    mutate(
      sim_file = glue(
        "{precompute_dir}/sims_valid_{fit_label}_{customer_id}.rds"
        )
      )

  syslog(
    glue("Configuring the out-of-sample validation simulations"),
    level = "INFO"
    )

  runsims_tbl <- model_index_tbl |>
    anti_join(precomputed_tbl, by = "sim_file")

  n_sims <- runsims_tbl |> nrow()

  if(n_sims > 0) {
    syslog(
      glue("Running {n_sims} out-of-sample simulations to {precompute_dir}"),
      level = "INFO"
      )

    plan(multisession)

    model_index_tbl <- runsims_tbl |>
      mutate(
        chunk_data = future_map2_int(
          cust_params, sim_file,
          run_simulations_chunk,

          sim_func = generate_pnbd_validation_transactions,

          .options = furrr_options(
            globals  = c(
              "calculate_event_times", "rgamma_mucv", "gamma_mucv2shaperate",
              "generate_pnbd_validation_transactions"
              ),
            packages   = c("tidyverse", "fs"),
            scheduling = Inf,
            seed       = sim_seed + 2
            ),

          .progress = TRUE
          )
        )
  }


  syslog(
    glue("Loading the calculated simulations"),
    level = "INFO"
    )


  model_valid_simstats_filepath <- glue("{data_dir}/{fit_label}_assess_valid_simstats_tbl.rds")


  if(!file_exists(model_valid_simstats_filepath)) {
    model_simdata_tbl <- model_index_tbl |>
      mutate(
        sim_data = map(
          sim_file, retrieve_sim_stats,

          .progress = "retrieve_valid_stats"
          )
        ) |>
      select(customer_id, sim_data) |>
      unnest(sim_data)

    model_simdata_tbl |> write_rds(model_valid_simstats_filepath, compress = "gz")

    rm(model_simdata_tbl)
  }

  assessment_lst <- list(
    model_simstats_filepath       = model_simstats_filepath,
    model_fit_simstats_filepath   = model_fit_simstats_filepath,
    model_valid_simstats_filepath = model_valid_simstats_filepath
    )


  return(assessment_lst)
}


create_model_assessment_plots <- function(obsdata_tbl, simdata_tbl) {

  ##
  ## First we check the counts of customer with more than 1 transaction
  ##
  obs_customer_count <- obsdata_tbl |>
    filter(tnx_count > 0) |>
    nrow()

  plotdata_tbl <- simdata_tbl |>
    filter(sim_tnx_count > 0) |>
    count(draw_id, name = "sim_customer_count")

  multi_customer_count_plot <- ggplot(plotdata_tbl) +
    geom_histogram(aes(x = sim_customer_count), bins = 50) +
    geom_vline(aes(xintercept = obs_customer_count), colour = "red") +
    labs(
      x = "Count of Multi-transaction Customers",
      y = "Frequency",
      title = "Comparison Plot of Simulated vs Observed Customer Counts",
      subtitle = "(observed value in red)"
      )


  ##
  ## Check the total count of transactions
  ##
  obs_total_count <- obsdata_tbl |>
    pull(tnx_count) |>
    sum()

  plotdata_tbl <- simdata_tbl |>
    count(draw_id, wt = sim_tnx_count, name = "sim_total_count")


  total_tnxcount_plot <- ggplot(plotdata_tbl) +
    geom_histogram(aes(x = sim_total_count), bins = 50) +
    geom_vline(aes(xintercept = obs_total_count), colour = "red") +
    labs(
      x = "Count of Total Transactions",
      y = "Frequency",
      title = "Comparison Plot of Simulated vs Observed Total Counts",
      subtitle = "(observed value in red)"
      )


  ###
  ### Check the quantiles of the transaction counts
  ###

  obs_quantiles_tbl <- obsdata_tbl |>
    filter(tnx_count > 0) |>
    reframe(
      prob_label = c("p10", "p25", "p50", "p75", "p90", "p99"),
      prob_value = quantile(tnx_count, probs = c(0.10, 0.25, 0.50, 0.75, 0.90, 0.99))
      )

  plotdata_tbl <- simdata_tbl |>
    filter(sim_tnx_count > 0) |>
    group_by(draw_id) |>
    summarise(
      p10 = quantile(sim_tnx_count, 0.10),
      p25 = quantile(sim_tnx_count, 0.25),
      p50 = quantile(sim_tnx_count, 0.50),
      p75 = quantile(sim_tnx_count, 0.75),
      p90 = quantile(sim_tnx_count, 0.90),
      p99 = quantile(sim_tnx_count, 0.99)
      ) |>
    pivot_longer(
      cols = !draw_id,
      names_to  = "prob_label",
      values_to = "sim_prob_values"
      ) |>
    inner_join(obs_quantiles_tbl, by = "prob_label")

  customer_tnxquant_plot <- ggplot(plotdata_tbl) +
    geom_histogram(aes(x = sim_prob_values), binwidth = 1) +
    geom_vline(aes(xintercept = prob_value), colour = "red") +
    facet_wrap(vars(prob_label), nrow = 2, scales = "free") +
    labs(
      x = "Quantile of Counts",
      y = "Frequency",
      title = "Comparison Plots of Transaction Count Quantiles"
      )


  ###
  ### Return these plots
  ###

  assess_plots_lst <- list(
    multi_plot = multi_customer_count_plot,
    total_plot = total_tnxcount_plot,
    quant_plot = customer_tnxquant_plot
    )

  return(assess_plots_lst)
}
