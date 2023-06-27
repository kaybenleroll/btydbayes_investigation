
#' This function runs the simulations for a given set of parameters.
#'
#' It is expected that a chunk of jobs are combined into a single job.
#'
#' @import tidyverse
#' @export run_simulations_chunk

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
