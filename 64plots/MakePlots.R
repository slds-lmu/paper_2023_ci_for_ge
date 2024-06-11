library(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(viridis)


source("shiny_app/setup.R")
source("64plots/helpers.R")
source("64plots/Plotfuns.R")

inducers = unique(ci_aggr$inducer)
sizes = unique(ci_aggr$size)
METHODS <- setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap"))

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
                             sep_reg_class = FALSE, stand=stand) 
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
                               sep_reg_class = FALSE, stand=stand)  
        initialize_nested_list(stand_loss_list_DGPs, c(as.character(size), inducer,input_y))
        initialize_nested_list(stand_loss_list_methods, c(as.character(size), inducer,input_y))
        
        
        other_loss_list_DGPs[[as.character(size)]][[inducer]][[input_y]][[loss]] <- output[["suspicious_DGPs"]]
        other_loss_list_methods[[as.character(size)]][[inducer]][[input_y]][[loss]] <- as.data.frame(output[["info"]])
      }
    }
  }
}


        
#### Making Plots
for (size in sizes) {
for (inducer in inducers) {
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
