library(rminions)
library(readr)

devtools::document("btydbayes")
devtools::install_local("btydbayes")

library(btydbayes)

#options(warn = 2, verbose = TRUE)

input_sims_tbl <- read_rds("data/tmp_input_sims_tbl.rds")

t1 <- Sys.time()
sims_tbl <- run_pnbd_simulations_chunk(
  input_param_tbl = input_sims_tbl
  )
t2 <- Sys.time()

print(t2 - t1)

remove.packages("btydbayes")
