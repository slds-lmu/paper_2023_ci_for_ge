source("Aggr_Plots/setup.R")

ci_aggr_red <- ci_aggr[method %nin% c("nested_cv_250","conservative_z_250"),]

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
Widths$width_std = Widths$width_median
Widths$width_std[Widths$task_type == "regr"] = Widths$sd[Widths$task_type == "regr"]^2 
Widths$width_std = Widths$width_std * sqrt(Widths$size / 10000)


x = Widths[measure %in% c("zero_one", "se")]
  x$width_std_r_sd = x$width_median / x$R_sd
x$width_std2 = x$width_median/ (x$R_sd / sqrt(x$size))

x[, let(
  width_std3 = width_median
)]

x$width_std3[x$task_type == "regr"] = x$width_std3[x$task_type == "regr"] / x$sd[x$task_type == "regr"] ^2
x$width_std3 = x$width_std3 * sqrt(x$size / 10000)

#x$width_std4 = x$width_std3 / 

ggplot(x[method %in% c("austern_zhou", "holdout_90", "corrected_t_10", "bayle_10_all_pairs"), ], aes(linetype = inducer, x = size, y = width_median / estimate_sd, color = method)) + 
  facet_wrap(vars(dgp), scales = "free") + 
  geom_line() 

  ggplot(x[method %in% c("corrected_t", "holdout_90") & inducer != "linear" & size >= 1000, ], aes(linetype = inducer, x = size, y = width_std_r_sd, color = method)) + 
  facet_wrap(vars(dgp), scales = "free") + 
  geom_line() 

ggplot(x[method %in% c("austern_zhou", "holdout_90"), ], aes(linetype = inducer, x = size, y = width_std3, color = method)) + 
  facet_wrap(vars(dgp), scales = "free") + 
  geom_line() + 
  labs(
    y = "Width (standardized by sd_y)"
  )

ggplot(x[method == "corrected_t_10", ], aes(x = size, y = , color = inducer)) + 
  facet_wrap(vars(dgp), scales = "free") + 
  geom_line() + 
  labs(
    title = "Corrected T; Standardized by R_sd"
  )


ggplot(x[method == "corrected_t_10", ], aes(x = size, y = width_std, color = inducer)) + 
  facet_wrap(vars(dgp), scales = "free") + 
  geom_line() + 
  labs(
    title = "Corrected T; Standardized by R_sd"
  )

ggplot(x[method == "corrected_t_10", ], aes(x = size, y = width_std2, color = inducer)) + 
  facet_wrap(vars(dgp), scales = "free") + 
  geom_line() + 
  labs(
    title = "Corrected T; Standardized by R_sd"
  )

ggplot(x[method %in% c("nested_cv", "austern_zhou", "holdout_90") & inducer == "random_forest", ], aes(linetype = method, x = size, y = width_std2, color = inducer)) + 
  facet_wrap(vars(dgp)) + 
  geom_line() + 
  labs(
    title = "Corrected T; Standardized by (R_sd / sqrt(n))"
  )

ggplot(x[method == "corrected_t_10", ], aes(x = estimate_sd, y = R_sd, color = inducer)) + 
  facet_wrap(vars(dgp)) + 
  geom_point() + 
  labs(
    title = "estimate_sd vs R_sd"
  )



Widths <- Widths[measure %in% translate_losses("Squared", "Zero-One") & 
                   as.character(dgp) %nin% susDGPs,
                 list(
                   mean_classif = mean(width[which(task_type=="classif")],na.rm=TRUE),
                   median_classif = median(width / estimate_sd),
                   mean_regr = mean(width/estimate_sd,
                                    na.rm=TRUE),
                   median_regr = median(width / estimate_sd#^2
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
       y = "Median (relative) width",
       fill = "Median width for") +
  theme_minimal() + scale_fill_manual(values=c("steelblue","slateblue"),
                                      labels=c(
                                        "median_classif" = "Classification",
                                        "median_regr" = "Regression" #target's sample variance)"
                                        
                                      )) +
  #geom_hline(yintercept = 0.2,color="red") + 
  geom_hline(yintercept = 01.4,color="red") +#,linetype="dotted") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dummy_p <- ggplot(data.frame(x=rep(c(1,2)),y=factor(rep(c(1,2),each=2))),
                  aes(x=x,y=y,linetype=y)) + geom_line(color="red") + theme_minimal() +
  scale_linetype_manual(
    values=c("solid","dotted"),
    labels= c("Cutoff for well-\nperforming methods","Adjusted cutoff for re-\ngression on small data")) +
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