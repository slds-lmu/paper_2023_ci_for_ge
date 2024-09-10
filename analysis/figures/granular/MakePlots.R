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
  "cv_10_all_pairs",
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
  "rocv_5",
  "rocv_5_5",
  "bccv",
  "bccv_bias"
)

ci_aggr = readRDS(here("results", "main", "ci_aggr.rds"))

INDUCERS = unique(ci_aggr$inducer)
SIZES = unique(ci_aggr$size)
DGPS = unique(ci_aggr$dgp)
LOSSES = unique(ci_aggr$loss)

#source("shiny_app_old/setup.R")
source(here("analysis", "figures", "granular", "helpers.R"))
source(here("analysis", "figures", "granular", "Plotfuns.R"))

stand = "mean" #or "Bayle"

### getting info
stand_loss_list_DGPs <- list()
stand_loss_list_methods <- list()

for (size in sizes) {
for (inducer in inducers) {
    for (input_y in c("Risk", "Expected Risk")) {
      output <- make_64plots(data = ci_aggr,
                             input_y = input_y, input_range = c(0, 1), input_size = size,
                             input_evaluation = "Coverage Frequency",
                             input_loss_regr = "Squared", input_loss_classif = "Zero-One",
                             methods = METHODS, dgps = DGPS, inducers = inducer,
                             sep_reg_class = FALSE,cutoff=0.5, stand=stand)
      
      initialize_nested_list(stand_loss_list_DGPs, c(as.character(size), inducer))
      initialize_nested_list(stand_loss_list_methods, c(as.character(size), inducer))
      
      stand_loss_list_DGPs[[as.character(size)]][[inducer]][[input_y]] <- output[["suspicious_DGPs"]]
      stand_loss_list_methods[[as.character(size)]][[inducer]][[input_y]] <- as.data.frame(output[["info"]])
    
    }
  }
}

other_loss_list_DGPs <- list()
other_loss_list_methods <- list()


for (size in sizes) {
for (inducer in inducers) {
    for (input_y in c("Risk", "Expected Risk")) {
      for (loss in unlist(LOSSES)) {
        loss_t = translate_losses(loss)
        output <- make_64plots(data = ci_aggr,
                               input_y = "Risk", input_range = c(0, 1), input_size = size,
                               input_evaluation = "Coverage Frequency",
                               input_loss_regr = loss, input_loss_classif = loss,
                               methods = METHODS, dgps = DGPS, inducers = inducer,
                               sep_reg_class = FALSE,cutoff=0.5, stand=stand)  
        initialize_nested_list(stand_loss_list_DGPs, c(as.character(size), inducer,input_y))
        initialize_nested_list(stand_loss_list_methods, c(as.character(size), inducer,input_y))
        
        other_loss_list_DGPs[[as.character(size)]][[inducer]][[input_y]][[loss]] <- output[["suspicious_DGPs"]]
        other_loss_list_methods[[as.character(size)]][[inducer]][[input_y]][[loss]] <- as.data.frame(output[["info"]])
      }
    }
  }
}

#,fun=function(x){data.frame(n_under_R=mean(x$n_under_R),n_underER=mean(x$n_under_ER),)})
Suspicious_DGPS_standard <- flatten_nested_list(stand_loss_list_DGPs,parent_names = list("standard:L2/0-1"))
names(Suspicious_DGPS_standard)[1:4] <- c("loss","size","inducer","target")
Suspicious_DGPS_other <- flatten_nested_list(other_loss_list_DGPs)
names(Suspicious_DGPS_other)[1:4] <- c("size","inducer","target","loss")
Suspicious_DGPS <- rbind(Suspicious_DGPS_standard,Suspicious_DGPS_other)
#save(Suspicious_DGPS,file="64plots/Data/Suspicious_DGPS.RData")

MethodsAll_standard <- flatten_nested_list(stand_loss_list_methods,parent_names = list("standard:L2/0-1"))
names(MethodsAll_standard)[1:4] <- c("loss","size","inducer","target")
MethodsAll_other <- flatten_nested_list(other_loss_list_methods)
names(MethodsAll_other)[1:4] <- c("size","inducer","target","loss")
AllMethods <- rbind(MethodsAll_standard,MethodsAll_other)
save(AllMethods,file="64plots/Data/AllMethods.RData")

        
#### Making Plots
for (size in SIZES) {
for (inducer in INDUCERS) {
    for (input_y in c("Risk", "Expected Risk")) {
      output <- make_64plots(data = ci_aggr,
        input_y = input_y, input_range = c(0, 1), input_size = size,
        input_evaluation = "Coverage Frequency",
        input_loss_regr = "Squared", input_loss_classif = "Zero-One",
        methods = METHODS, dgps = DGPS, inducers = inducer,
        sep_reg_class = FALSE, stand=stand) 
      output[["plot"]] + + ggtitle(
        paste("inducer: ", inducer, ", size: ", size, ", target: ", input_y, ", loss: L2/0-1")
      )
      ggsave(here("64plots", "PNGs", "classic_losses", paste0(inducer, "_", size, "_", gsub(" ", "", input_y), ".png")), width = 12, height = 14)
    }
  }
}

# not also for the different loss functions. Here we need to split by task type, otherwise we have 3 * 4 = 12 combinations

for (size in sizes) {
for (inducer in inducers) {
    for (input_y in c("Risk", "Expected Risk")) {
      for (loss in unlist(LOSSES)) {
        loss_t = translate_losses(loss)
        output <- make_64plots(data = ci_aggr,
          input_y = "Risk", input_range = c(0, 1), input_size = size,
          input_evaluation = "Coverage Frequency",
          input_loss_regr = loss, input_loss_classif = loss,
          methods = METHODS, dgps = DGPS, inducers = inducer,
          sep_reg_class = FALSE, stand=stand)
        output[["plot"]] + ggtitle(
          paste("inducer: ", inducer, ", size: ", size, ", target: ", input_y, ", loss: ", loss)
        )
        ggsave(here("64plots", "PNGs", "other_losses", paste0(inducer, "_", size, "_", gsub(" ", "", input_y), "_", loss_t, ".png")), width = 12, height = 14)
      }
    }
  }
}
