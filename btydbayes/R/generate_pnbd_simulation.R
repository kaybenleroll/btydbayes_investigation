
#' This function generates customer simulation parameters based on the P/NBD
#' model
#'
#' @importFrom tibble as.tibble tibble enframe
#' @importFrom dplyr select arrange mutate filter group_by ungroup
#' @importFrom purrr pmap
#' @importFrom tidyr unnest
#' @export generate_pnbd_customer_simulation_params
#' @export generate_pnbd_customer_transaction_data
#' @export generate_pnbd_individual_transactions
#' @export generate_transaction_metadata
#' @export generate_pnbd_validation_transactions

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
        tnx_amount    = tnx_amounts |> round(2)
        )

    if(nrow(tnxdata_tbl) > 0) {
      tnxdata_tbl <- tnxdata_tbl |>
        filter(
          tnx_timestamp <= end_dttm
          )
    }

  } else {

    tnxdata_tbl <- tibble(
        tnx_timestamp = start_dttm,
        tnx_amount    = 0
        ) |>
      slice(0)

  }

  return(tnxdata_tbl)
}
