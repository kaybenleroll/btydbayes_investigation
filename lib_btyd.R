

generate_customer_cohort_data <- function(n_customers, first_date, last_date) {
  tnx_dates <- seq(first_date, last_date, by = "day")

  customer_cohort_tbl <- tnx_dates %>%
    enframe(name = NULL, value = "first_tnx_date") %>%
    slice_sample(n = n_customers, replace = TRUE) %>%
    arrange(first_tnx_date) %>%
    group_by(format(first_tnx_date, "%Y%m")) %>%
    mutate(
      customer_id = sprintf("C%s_%04d", format(first_tnx_date, "%Y%m"), 1:n()),
      cohort_qtr  = first_tnx_date %>% as.yearqtr() %>% as.character(),
      cohort_ym   = first_tnx_date %>% format("%Y %m")
      ) %>%
    ungroup() %>%
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

  customer_basic_parameters_tbl <- customer_cohort_data_tbl %>%
    select(customer_id, cohort_qtr, cohort_ym, first_tnx_date) %>%
    arrange(first_tnx_date, customer_id) %>%
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
  customer_transactions_tbl <- sim_params_tbl %>%
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
      ) %>%
    select(
      customer_id, cohort_qtr, cohort_ym, sim_data
      ) %>%
    unnest(sim_data)

  return(customer_transactions_tbl)
}


generate_pnbd_individual_transactions <- function(
    lifetime, tnx_rate, amt_mn, amt_cv, first_date,
    customer_id = customer_id, final_date = final_date) {

  obs_weeks <- difftime(final_date, first_date, units = "weeks") %>% as.numeric()

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
    tnx_amount    = tnx_amounts %>% round(2)
    )

  return(sim_tnx_tbl)
}


generate_transaction_metadata <- function(data_tbl) {
  transactions_tbl <- data_tbl %>%
    arrange(tnx_timestamp) %>%
    group_by(tnx_date = as.Date(tnx_timestamp)) %>%
    mutate(
      invoice_id = sprintf("T%s-%04d", format(tnx_date, "%Y%m%d"), 1:n())
    ) %>%
    ungroup() %>%
    select(customer_id, tnx_timestamp, invoice_id, tnx_amount)

  return(transactions_tbl)
}


