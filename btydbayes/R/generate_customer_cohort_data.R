
#' This function generates customer cohort data
#'
#' @importFrom tibble enframe
#' @importFrom dplyr slice_sample arrange group_by mutate ungroup select
#' @importFrom zoo as.yearqtr
#' @export calculate_transaction_cbs_data

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


