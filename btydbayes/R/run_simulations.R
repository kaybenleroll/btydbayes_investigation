
#' This function runs the simulations for a given set of parameters.
#'
#' It is expected that a chunk of jobs are combined into a single job.
#'
#' @importFrom tibble as.tibble tibble enframe
#' @importFrom magrittr divide_by
#' @importFrom dplyr select arrange mutate filter group_by ungroup rename
#' @importFrom dplyr inner_join semi_join anti_join group_nest
#' @importFrom dplyr slice_max
#' @importFrom purrr pmap map map_int
#' @importFrom tidyr unnest
#' @export run_pnbd_simulations_chunk

run_pnbd_simulations_chunk <- function(input_param_tbl) {

  sim_param_tbl <- input_param_tbl |>
    mutate(across(contains("_dttm"), \(x) as.POSIXct(x, tz = "UTC"))) |>
    mutate(across(contains("_date"), \(x) as.POSIXct(x, tz = "UTC")))


  select_last_date <- function(data_tbl) {
    last_tbl <- data_tbl |>
      slice_max(n = 1, order_by = tnx_timestamp, with_ties = FALSE) |>
      select(sim_tnx_last = tnx_timestamp)

    return(last_tbl)
  }

  simdata_tbl <- sim_param_tbl |>
    group_nest(customer_id, draw_id, .key = "sim_params") |>
    mutate(
      sim_data      = map(sim_params, generate_pnbd_validation_transactions),
      sim_tnx_count = map_int(sim_data, nrow),
      last_data     = map(sim_data, select_last_date)
    ) |>
    unnest(last_data, keep_empty = TRUE) |>
    unnest(sim_params) |>
    select(customer_id, draw_id, sim_data, sim_tnx_count, sim_tnx_last)

  return(simdata_tbl)
}
