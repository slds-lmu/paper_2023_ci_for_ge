library(dplyr)
library(ggplot2)
library(here)
library(data.table)
library(mlr3oml)

source("64plots/DetectiveWork/MetaInfo.R")
dgps <- readr::read_csv("shiny_app/dgps.csv")

never_sus
sometimes_sus

options(mlr3oml.cache = TRUE)

for(name in never_sus){
  ID <- as.numeric(names(which(data_names==name)))
  assign(paste0("nonsus_",name),odt(ID, parquet = TRUE))
  #assign(paste0("nonsus_",name,"_dat"),get(paste0("nonsus_",name))$data[[get(paste0("nonsus_",name))$target_names]])
}

for(name in sometimes_sus){
  ID <- as.numeric(names(which(data_names==name)))
  assign(paste0("sus_",name),odt(ID, parquet = TRUE))
  #assign(paste0("sus_",name,"_dat"),get(paste0("sus_",name))$data)
}




get_type <- function(x){
  tname <- get(x)$target_names
  return(class(get(x)$data[[tname]]))
}
sapply(paste0("nonsus_",never_sus), function(x){get(x)$target_names})
sapply(paste0("sus_",sometimes_sus), function(x){get(x)$target_names})







