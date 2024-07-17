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

source("shiny_app_old/setup.R")
source("Aggr_Plots/Plotfuns.R")

sds <- readRDS("Aggr_Plots/sds.rds")
sds_tbls <- data.table(
  dgp = names(sds),
  sd = unlist(sds)
)

inducers = unique(ci_aggr$inducer)

DGPs <- setdiff(DGPS,c("adult","video_transcoding","physiochemical_protein","chen_10_null"))
