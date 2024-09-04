library(here)
library(ggplot2)
library(data.table)
library(mlr3misc)

theme_set(theme_minimal())
ncv = readRDS(here("results", "ablation", "ncv_cheap_aggr.rds"))
ncv = ncv[dgp %nin% c("chen_10_null", "adult", "physiochemical_protein"), ]

ggplot(ncv, aes(x = reps_outer, y = cov_R, color = dgp)) + 
  facet_grid(vars(size), vars(learner), scales = "free") + 
  geom_line()

ggplot(ncv[task_type == "classif", ], aes(x = reps_outer, y = mean_width, color = dgp)) + 
  facet_grid(vars(size), vars(learner), scales = "free") + 
  geom_line()

unique(ncv$dgp)
#[1] "bates_classif_100" "bates_classif_20"  "bates_regr_100"    "bates_regr_20"     "breast"           
#[6] "chen_10"           "colon"             "covertype"         "diamonds"          "electricity"      
# [1] "friedman1"         "higgs"             "prostate"          "sgemm_gpu"         "video_transcoding"


ggplot(ncv[size > 100, ], aes(x = reps_outer, y = cov_R, color = dgp)) + 
  facet_grid(vars(size), vars(learner), scales = "free") + 
  geom_line()

ggplot(ncv[size > 100 & task_type == "classif", ], aes(x = reps_outer, y = mean_width, color = dgp)) + 
  facet_grid(vars(size), vars(learner), scales = "free") + 
  geom_line()

conz = readRDS(here("results", "ablation", "conz_cheap_aggr.rds"))
conz = conz[dgp %nin% c("chen_10_null", "adult", "physiochemical_protein"), ]


ggplot(conz[outer_reps == 12, ], aes(x = inner_reps, y = cov_R, color = dgp)) + 
  facet_grid(vars(size), vars(learner), scales = "free") + 
  geom_line()


 ggplot(conz[task_type == "classif" & inner_reps == 10, ], aes(x = outer_reps, y = cov_R, color = dgp)) + 
  facet_grid(vars(size), vars(learner), scales = "free") + 
  geom_line()

