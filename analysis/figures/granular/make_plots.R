library(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(viridis)

DATA_OVERVIEW <- data.table::fread(here("data", "dgps.csv"))[, -"openml_id"]

METHODS = c(
  "holdout_66",
  "holdout_90",
  "cort_10",
  "cort_50",
  "cort_100",
  "cv_5_within",
  "cv_5_allpairs",
  "cv_10_within",
  "cv_10_allpairs",
  "cv_n",
  "52cv",
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
  "lsb_50",
  "lsbp_100",
  "tsb_200_10",
  "ncv_200_5",
  "conz_10_15",
  "conz_50_50",
  "conz_10_5",
  "ncv_10_5",
  "ncv_3_5",
  "rocv_5",
  "rocv_5_5",
  "bccv",
  "bccv_bias"
)

ci_aggr = readRDS(here("results", "main", "ci_aggr.rds"))
ci_aggr$dgp = as.factor(gsub("simulated_", "", ci_aggr$dgp))

get_inducer_both = function(inducer) {
  switch(inducer,
    lm_or_logreg = "Linear/Logistic Regression",
    decision_tree = "Decision Tree",
    random_forest = "Random Forest",
    ridge_lm_or_logreg = "Ridge-penalized Linear/Logistic Regression"
  )
}
get_inducer = function(inducer) {
  switch(inducer,
    lm_or_logreg = "Linear/Logistic Regression",
    decision_tree = "Decision Tree",
    random_forest = "Random Forest",
    ridge_lm_or_logreg = "Ridge-penalized Linear/Logistic Regression"
  )
}


INDUCERS = unique(ci_aggr$inducer)
SIZES = unique(ci_aggr$size)
DGPS = unique(ci_aggr$dgp)
LOSSES = unique(ci_aggr$loss)

source(here("analysis", "figures", "granular", "plot_funs.R"))

# standard losses

for (size in SIZES) {
  for (inducer in INDUCERS) {
    for (input_y in c("Risk", "Expected Risk")) {
      for (loss in LOSSES) {
        output = make_64plots(
          data = ci_aggr,
          input_y = input_y,
          input_size = size,
          input_loss_regr = "se",
          input_loss_classif = "zero_one",
          methods = METHODS,
          dgps = DGPS,
          inducers = inducer
        )
        output = output + ggtitle(
          paste0("Inducer: ", get_inducer_both(inducer), ", n_D: ", size, ", Target: ", input_y, ", Loss: se/zero_one\n",
          "Values in brackets are total resampling iterations")
        )
        ggsave(
          plot = output, 
          here("figures", "granular", "standard_losses", paste0(inducer, "_", size, "_", gsub(" ", "", input_y), ".png")), width = 14, height = 14)
      }
    }
  }
}

# all losses
for (size in SIZES) {
  for (inducer in INDUCERS) {
    for (input_y in c("Risk", "Expected Risk")) {
      for (loss in LOSSES) {
        output = make_64plots(
          data = ci_aggr,
          input_y = input_y,
          input_size = size,
          input_loss_regr = loss,
          input_loss_classif = loss,
          methods = METHODS,
          dgps = DGPS,
          inducers = inducer
        )
        output = output + ggtitle(
          paste0("Inducer: ", get_inducer(inducer), ", n_D: ", size, ", Target: ", input_y, ", Loss: ", loss, 
          "\nValues in brackets are total resampling iterations.")
        )
        ggsave(plot = output,
          here("figures", "granular", "other_losses", paste0(inducer, "_", size, "_", gsub(" ", "", input_y), "_", loss, ".png")), width = 14, height = 14)
      }
    }
  }
}
