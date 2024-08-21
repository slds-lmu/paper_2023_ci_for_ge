source("Aggr_Plots/setup.R")
source("Aggr_Plots/NewNames.R")

ci_aggr_red <- ci_aggr[method %nin% c("nested_cv_250","conservative_z_250"),]
ci_aggr_red <- replace_names(ci_aggr_red,"method",MethodNames)

UC <- ci_aggr_red[measure %in% translate_losses("Squared", "Zero-One") & 
                as.character(dgp) %nin% susDGPs,
              list(
                umean_R = mean(0.95-pmin(cov_R,0.95),na.rm=TRUE), 
                umean_ER = mean(0.95-pmin(cov_ER,0.95),na.rm=TRUE), 
                umean_PQ = mean(0.95-pmin(cov_PQ,0.95),na.rm=TRUE) 
              ),
              by = c("method")]

UC <- UC %>%
  mutate(method = fct_reorder(method, umean_R, .desc = TRUE))

UC_plot <- pivot_longer(UC,cols=c(umean_R,umean_ER,umean_PQ),names_to = "of", values_to = "value") %>%
  mutate(of = factor(of,
                                   levels=c("umean_ER","umean_R","umean_PQ")))

p1 <- ggplot(UC_plot, aes(x = method, y = value, fill=of)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "",
       x = "Method",
       y = "Average undercoverage",
       fill = "Undercoverage of") +
  theme_minimal() + scale_fill_manual(values=c("darkblue","forestgreen","slategray"),
                                      labels=c("umean_ER"="Expected Risk",
                                               "umean_R"="Risk", 
                                               "umean_PQ"="Proxy Quantity (if applicable)"
                                      )) +
  geom_hline(yintercept = 0.1,color="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


################################################################################

#ci_aggr_red_rel <- ci_aggr_red %>%
#                  mutate(rel_width=width/R_sd/ER)


Widths <- merge(ci_aggr_red,sds_tbls,by="dgp")

Widths <- Widths[measure %in% translate_losses("Squared", "Zero-One") & 
                   as.character(dgp) %nin% susDGPs,
                 list(
                   mean_classif = mean(width[which(task_type=="classif")],na.rm=TRUE),
                   median_classif = median(width[which(task_type=="classif")]/(estimate_sd[which(task_type=="classif")])
                                           ,na.rm = TRUE),
                   mean_regr = mean(width[which(task_type=="regr")]/R_sd[which(task_type=="regr")]#^2
                                    ,
                                    na.rm=TRUE),
                   median_regr = median(width[which(task_type=="regr")]/(estimate_sd[which(task_type=="regr")])#^2
                                        ,
                                        na.rm = TRUE)
                 ),
                 by = c("method")]

Widths <- Widths %>%
  mutate(method = factor(method, levels=levels(UC$method)))


Widths_plot <- pivot_longer(Widths,cols=c(median_classif,median_regr),
                            names_to = "aggr", values_to = "value")



p2 <- ggplot(Widths_plot, aes(x = method, y = value, fill=aggr)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "",
       x = "Method",
       y = "Median relative width",
       fill = "Median width for") +
  theme_minimal() + scale_fill_manual(values=c("steelblue","slateblue"),
                                      labels=c(
                                        "median_classif" = "Classification",
                                        "median_regr" = "Regression" #target's sample variance)"
                                        
                                      )) +
  #geom_hline(yintercept = 0.2,color="red") + 
  geom_hline(yintercept = 8,color="red") +#,linetype="dotted") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

################################################################################
dummy_p <- ggplot(data.frame(x=c(1,1),y=factor(c(1,1))),
                  aes(x=x,y=y,linetype=y)) + geom_line(color="red") + theme_minimal() +
                  scale_linetype_manual(
                    values=c("solid"),#,"dotted"),
                    labels= c("Cutoff for well-\nperforming methods")) +#,"Adjusted cutoff for re-\ngression on small data")) +
                    labs(linetype="") + 
                    theme(legend.key.height = unit(3, "line"),
                          legend.key.width = unit(2, "line"),
                          legend.text = element_text(size=10))

legend <- ggpubr::get_legend(dummy_p)

P1 <- p1+scale_x_discrete(position = "top")+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5))+
  theme(legend.position = "top",
        axis.title.y = element_text(size=14),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

P2 <- p2 + scale_y_reverse() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size=14),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

plot_grid(
  P1/P2,  
  legend,
  nrow = 1, rel_widths = c(1, 0.2)  # Adjust relative widths as needed
)

ggsave("Aggr_Plots/PNGs/FirstRound_different_stand.png",width=11.5,height=9)

#ggsave("Aggr_Plots/PNGs/FirstRound.png",width=11.5,height=9)


#ggplot(Widths_plot, aes(x = method, y = value, fill=aggr)) +
#  geom_bar(stat = "identity", position = position_dodge()) +
#  labs(title = "",
#       x = "Method",
#       y = "Median (relative) width",
#       fill = "Median width for") +
#  theme_minimal() + scale_fill_manual(values=c("steelblue","slateblue"),
#                                      labels=c(
#                                        "median_classif" = "Classification",
#                                        "median_regr" = "Regression (relative to SD of risk)" #target's sample variance)"
#                                        
#                                      )) +
#  geom_hline(yintercept = 1.96,color="red") + 
#  #geom_hline(yintercept = 0.4,color="red",linetype="dotted") + 
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


