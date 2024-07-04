library(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(viridis)
library(tidyr)


source("shiny_app_old/setup.R")
source("Aggr_Plots/Plotfuns.R")

inducers = unique(ci_aggr$inducer)
sizes = unique(ci_aggr$size)
Mlarge <- c("holdout_90","bayle_10_all_pairs","corrected_t_10","oob_1000","632plus_1000","dietterich")
Msmall <- c("conservative_z","nested_cv")

DGPs <- setdiff(DGPS,c("adult","video_transcoding","physiochemical_protein","chen_10_null"))

aggr_plot(ci_aggr, c(Mlarge,Msmall), inducers, DGPs, "Squared", "Zero-One", ylims=c(0.5,1))


#aggr_plot(ci_aggr, Msmall, inducers, DGPs, "Squared", "Zero-One", ylims=c(0.8,1))
