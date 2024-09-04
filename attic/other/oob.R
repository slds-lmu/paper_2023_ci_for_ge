library(here)
library(data.table)
library(ggplot2)

ci = readRDS(here("results", "ci_aggr.rds"))

oob = ci[startsWith(as.character(method), "oob") & size <= 500, ]
oob$repeats = as.integer(gsub(oob$repeats, pattern = "oob_", replace = ""))

oob = oob, list(cov_R = mean(cov_R))

ggplot(, aes(x = size, y = cov_R, color = repeats)) + 
  facet_wrap(vars(learner)) + 
  geom_line()
