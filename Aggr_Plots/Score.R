library(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(viridis)
library(tidyr)
library(forcats)
library(DescTools)
library(gridExtra)
library(ggpubr)
library(ggeasy)

source("shiny_app_old/setup.R")

UC <- ci_aggr[measure %in% translate_losses("Squared", "Zero-One") & 
                as.character(dgp) %in% setdiff(DGPS,c("adult","video_transcoding","physiochemical_protein","chen_10_null")),
              list(
                umean_R = mean(0.95-pmin(cov_R,0.95),na.rm=TRUE), 
                umean_ER = mean(0.95-pmin(cov_ER,0.95),na.rm=TRUE), 
                umean_PQ = mean(0.95-pmin(cov_PQ,0.95),na.rm=TRUE) 
              ),
              by = c("method")]
UC <- UC %>%
  mutate(method = fct_reorder(method, umean_R, .desc = TRUE))

UC_plot <- pivot_longer(UC,cols=c(umean_R,umean_ER,umean_PQ),names_to = "undercoverage_of", values_to = "value") %>%
                        mutate(undercoverage_of = factor(undercoverage_of,
                                                         levels=c("umean_ER","umean_R","umean_PQ")))

p1 <- ggplot(UC_plot, aes(x = method, y = value, fill=undercoverage_of)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "",
       x = "Method",
       y = "Average\n undercoverage") +
  theme_minimal()+
  geom_hline(yintercept = 0.1,color="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


UC <- ci_aggr[measure %in% translate_losses("Squared", "Zero-One") & 
                as.character(dgp) %in% setdiff(DGPS,c("adult","video_transcoding","physiochemical_protein","chen_10_null")),
              list(
                under_R = ifelse(0.95-pmin(cov_R,0.95),na.rm=TRUE), 
                under_ER = mean(0.95-pmin(cov_ER,0.95),na.rm=TRUE), 
                umean_PQ = mean(0.95-pmin(cov_PQ,0.95),na.rm=TRUE) 
              ),
              by = c("method")]
UC <- UC %>%
  mutate(method = fct_reorder(method, umean_R, .desc = TRUE))

UC_plot <- pivot_longer(UC,cols=c(umean_R,umean_ER,umean_PQ),names_to = "undercoverage_of", values_to = "value") %>%
  mutate(undercoverage_of = factor(undercoverage_of,
                                   levels=c("umean_ER","umean_R","umean_PQ")))

p2 <- ggplot(UC_plot, aes(x = method, y = value, fill=undercoverage_of)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "",
       x = "Method",
       y = "Average\n undercoverage") +
  theme_minimal()+
  geom_hline(yintercept = 0.1,color="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




################################################################################

#ci_aggr_rel <- ci_aggr %>%
#                  mutate(rel_width=width/R_sd/ER)
  
  
Widths <- ci_aggr[measure %in% translate_losses("Zero-One") & 
                as.character(dgp) %in% setdiff(DGPS,c("adult","video_transcoding","physiochemical_protein","chen_10_null")),
              list(
              mean=mean(width,na.rm=TRUE),
              median=median(width,na.rm = TRUE)
              ),
              by = c("method")]

Widths <- Widths %>%
  mutate(method = factor(method, levels=levels(UC$method)))
Widths$method[which(Widths$mean<0.2)]


Widths_plot <- pivot_longer(Widths,cols=c(mean,median),names_to = "aggr", values_to = "value")



p3 <- ggplot(Widths_plot, aes(x = method, y = value, fill=aggr)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "",
       x = "Method",
       y = "Average relative width\n for classification") +
  theme_minimal()+
  geom_hline(yintercept = 0.2,color="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggarrange(p1+xlab("")+scale_x_discrete(position = "top")+
          theme(axis.text.x = element_text(angle = 270, vjust = 0.5)),
          p2+scale_y_reverse(),nrow=2)






