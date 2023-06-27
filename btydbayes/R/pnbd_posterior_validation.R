
#' This function calculates posterior validation data for the P/NBD model
#'
#' @importFrom tibble as.tibble tibble enframe
#' @importFrom magrittr divide_by
#' @importFrom dplyr select arrange mutate filter group_by ungroup rename
#' @importFrom dplyr inner_join semi_join anti_join
#' @importFrom purrr pmap
#' @importFrom tidyr unnest
#' @importFrom tidybayes spread_draws gather_draws recover_types
#' @importFrom ggplot2 geom_histogram geom_hline geom_errorbar
#' @export create_pnbd_posterior_validation_data
#' @export construct_pnbd_posterior_statistics

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
    gather_draws(lambda[customer_id], mu[customer_id], p_alive[customer_id]) |>
    ungroup() |>
    pivot_wider(
      id_cols = c(customer_id, .draw),
      names_from = .variable,
      values_from = .value
      ) |>
    inner_join(fitdata_tbl, by = "customer_id") |>
    select(
      customer_id, first_tnx_date, draw_id = .draw,
      post_lambda = lambda, post_mu = mu, p_alive
      )

  return(post_stats_tbl)
}
