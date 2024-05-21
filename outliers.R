library(data.table)
library(here)

ci = readRDS(here("results", "final.rds"))

ci$width = ci$upper - ci$lower

