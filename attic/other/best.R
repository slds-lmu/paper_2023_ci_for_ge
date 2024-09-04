library(here)
library(data.table)

conz = readRDS(here("results", "ablation", "conz_cheap_aggr.rds"))
conz = conz[inner_reps == 5 & outer_reps == 12, ]
conz$method = "conservative_z"
conz$inner_reps = NULL
conz$outer_reps = NULL

ncv = readRDS(here("results", "ablation", "ncv_cheap_aggr.rds"))
ncv = ncv[reps_outer == 3, , ]
ncv$method = "nested_cv"
ncv$reps_outer = NULL

cort = readRDS(here("results", "ablation", "cort_aggr.rds"))
cort = cort[reps == 20 & ratio == 0.9, ]
cort$method = "corrected_t"
cort$ratio = NULL
cort$reps = NULL

tbl = rbindlist(list(conz, ncv, cort))

