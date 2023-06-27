

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
