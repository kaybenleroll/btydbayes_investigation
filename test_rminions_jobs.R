library(rminions)
library(readr)
library(redux)


redux_conn <- hiredis(host = "gru_svr")

input_sims_tbl <- read_rds("data/tmp_input_sims_tbl.rds")

rminions::sendMessage(
  conn            = redux_conn,
  package         = 'btydbayes',
  func            = 'run_pnbd_simulations_chunk',
  input_param_tbl = input_sims_tbl,
  useJSON         = FALSE
  )


# results_lst <- redux_conn$RPOP("errorQueue")
