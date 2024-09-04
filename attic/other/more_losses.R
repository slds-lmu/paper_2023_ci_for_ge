library(data.table)
library(here)
library(ggplot2)

more_losses = readRDS(here("results", "more_losses.rds"))
table(more_losses$repl) |> unique()
table(more_losses$task)

more_losses$task = ifelse(more_losses$task == "phyisiochemical_protein",
                          "physiochemical_protein", more_losses$task)
more_losses$learner = ifelse(more_losses$learner == "ridge2", "ridge", more_losses$learner)
setnames(more_losses, "resampling", "proxy_resampling")
more_losses$method = ifelse(more_losses$method == "diettrich", "dietterich", more_losses$method)
more_losses[, let(
  
)]

ci = readRDS(here("results", "ci.rds"))
ci = ci[size == 500 & task_type == "regr", ]
