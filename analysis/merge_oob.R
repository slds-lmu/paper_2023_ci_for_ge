library(here)
library(data.table)

ci = readRDS(here("results", "ci_for_ge.rds"))

oob = readRDS(here("results", "oob.rds"))
oob_632 = readRDS(here("results", "oob_632.rds"))
oob_632$info = list(list(NULL))
oob = rbindlist(list(oob, oob_632), use.names = TRUE)
  
setnames(oob, "resampling", "proxy_resampling")
oob$info = list(list(NULL))
ci_oob = merge(oob,
  ci[method == "oob_100", c("measure", "learner", "task", "size", "repl", "R", "PQ")],
  by = c("measure", "learner", "task", "size", "repl")
)

ridge = readRDS(here("results", "ridge.rds"))
lm = readRDS(here("results", "lm.rds"))

final = rbindlist(list(ci, ci_oob, ridge, lm), use.names = TRUE)
saveRDS(final, here("results", "ci.rds"))