library(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(viridis)


source("shiny_app_old/setup.R")
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