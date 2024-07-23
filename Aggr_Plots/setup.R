library(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(viridis)
library(RColorBrewer)
library(tidyr)
library(forcats)
library(DescTools)
library(gridExtra)
library(ggpubr)
library(ggeasy)
library(patchwork)
library(cowplot)

source("Aggr_Plots/Plotfuns.R")

ci_aggr_orig <- readRDS("Aggr_Plots/ablation/ci_aggr.rds")

ci_aggr_orig$inducer = mlr3misc::map_chr(ci_aggr_orig$learner, function(l) {
  switch(as.character(l),
         "linear" = "lm_or_logreg",
         "rpart" = "decision_tree",
         "ranger" = "random_forest",
         "ridge" = "ridge_lm_or_logreg"
  )
})

ci_aggr_orig$learner = NULL
ci_aggr_orig$dgp = ci_aggr_orig$task
ci_aggr_orig$task = NULL


ci_aggr <- ci_aggr_orig
ci_aggr[, let(dgp = droplevels(dgp))]

sds <- readRDS("Aggr_Plots/sds.rds")
sds_tbls <- data.table(
  dgp = names(sds),
  sd = unlist(sds)
)


capitalize <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

translate_target <- function(target) {
  switch(target,
         "Risk" = "R",
         "Expected Risk" = "ER",
         "Proxy Quantity" = "PQ",
         stop("not available")
  )
}

PQ_METHODS <- c(
  "bayle_5_within",
  "bayle_5_all_pairs",
  "bayle_10_within",
  "bayle_10_all_pairs",
  "austern_zhou",
  "austern_zhou_rep",
  "holdout_66",
  "holdout_90"
)


DGPS <- c(
  "higgs",
  "adult",
  "covertype",
  "bates_classif_20",
  "colon",
  "bates_classif_100",
  "breast",
  "prostate",
  "electricity",
  "diamonds",
  "physiochemical_protein",
  "bates_regr_20",
  "friedman1",
  "bates_regr_100",
  "chen_10_null",
  "chen_10",
  "sgemm_gpu",
  "video_transcoding"
)

INDUCERS <- c("lm_or_logreg", "ridge_lm_or_logreg", "random_forest", "decision_tree")

METHODS <- c(
  "holdout_66",
  "holdout_90",
  "corrected_t_10",
  "corrected_t_100",
  "bayle_5_all_pairs",
  "bayle_10_all_pairs",
  "dietterich",
  "nested_cv",
  "conservative_z",
  "ts_bootstrap",
  "bayle_loo",
  "austern_zhou",
  "austern_zhou_rep",
  "bccv",
  "bccv_bias",
  "oob_1000",
  "632plus_1000"
)
METHODS <- c(
  "holdout_66",
  "holdout_90",
  "corrected_t_10",
  "corrected_t_50",
  "corrected_t_100",
  "bayle_5_within",
  "bayle_5_all_pairs",
  "bayle_10_within",
  "bayle_10_all_pairs",
  "bayle_loo",
  "dietterich",
  "oob_10",
  "oob_50",
  "oob_100",
  "oob_500",
  "oob_1000",
  "632plus_10",
  "632plus_50",
  "632plus_100",
  "632plus_500",
  "632plus_1000",
  "ls_bootstrap_50",
  "ls_bootstrap_100",
  "ts_bootstrap",
  "nested_cv",
  "conservative_z",
  "austern_zhou",
  "austern_zhou_rep",
  "bccv",
  "bccv_bias"
) 
DEFAULT_METHODS = setdiff(METHODS, c(
  "632plus_500",
  "oob_500",
  "corrected_t_50",
  "bayle_5_within",
  "bayle_10_within",
  "oob_10",
  "632plus_10",
  "oob_50",
  "632plus_50",
  "oob_100",
  "ls_bootstrap_50",
  "632plus_100"
))

CHEAP_METHODS <- c(
  "holdout_66",
  "holdout_90",
  "corrected_t_10",
  "corrected_t_50",
  "corrected_t_100",
  "bayle_5_within",
  "bayle_5_all_pairs",
  "bayle_10_within",
  "bayle_10_all_pairs",
  "dietterich",
  "oob_10",
  "632plus_10",
  "oob_50",
  "ls_bootstrap_50",
  "632plus_50",
  "oob_100",
  "ls_bootstrap_100",
  "632plus_100",
  "ts_bootstrap"
)

LOSSES <- list(
  regr = c("Squared", "Absolute", "Perc. Sq.", "Std. Sq."),
  classif = c("Zero-One", "Brier", "Log-Loss")
)

translate_loss <- function(loss) {
  switch(loss,
         "Squared" = "se",
         "Absolute" = "ae",
         "Perc. Sq." = "percentual_se",
         "Std. Sq." = "standardized_se",
         "Zero-One" = "zero_one",
         "Brier" = "bbrier",
         "Log-Loss" = "logloss"
  )
}

translate_losses <- function(...) {
  sapply(list(...), translate_loss)
}










inducers = unique(ci_aggr$inducer)

susDGPs <- c("video_transcoding","physiochemical_protein","chen_10_null")
DGPs <- setdiff(DGPS,susDGPs)
