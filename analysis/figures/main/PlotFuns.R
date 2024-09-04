aggr_plot <- function(data, methods, inducers, DGPs, loss_regr, loss_classif, ylims=c(0,1),ncols=2){
  data = data[as.character(method) %in% methods & as.character(dgp) %in% DGPs  &
              loss %in% translate_losses(loss_regr, loss_classif),
              list(
                y_R = mean(cov_R),
                y_ER = mean(cov_ER),
                y_PQ = mean(cov_PQ)
              ), by = c("inducer", "method", "size")]
  min_size = min(ylims)
  max_size = max(ylims)

  #if(any(is.na(data$y_PQ))){
  #  columns <- c("y_R","y_ER")
  #}else{
   columns <- c("y_R","y_ER","y_PQ")
  #}
  plotdat <- pivot_longer(data, cols = all_of(columns),
                          names_to = "coverage_of", values_to = "value")

  output <- ggplot(plotdat,aes(x=size,y=value,color=inducer))+
    geom_hline(yintercept = 0.95,color="black") +
    geom_line(aes(linetype=coverage_of),size=0.33) +
    facet_wrap(method ~.,scales="free_x", ncol=ncols) +
    ylim(min_size,max_size) +
    theme_bw()

  return(output)
}

################################################################################
merge_plots <- function(Plots,heights){
p1 <- Plots[[1]] + theme(legend.position = "bottom",
                        legend.box = "vertical",
                        plot.margin = margin(t=-10,l=5,r=5))
p2 <- Plots[[2]] + easy_remove_x_axis() +
  theme(legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(l=5,r=5)
  )

 return(ggarrange(p2,p1,nrow=2,heights=heights))
}

################################################################################

aggr_plot_conz <- function(data, inducers, DGPs, ylims=c(0,1), SDs){
  data <- merge(data,SDs,by="dgp")
  data = data[as.character(dgp) %in% DGPs & inner_reps %in% c(5,25,45),
              list(
                y_R = mean(cov_R),
                y_ER = mean(cov_ER),
                median_classif = median(median_width[which(task_type=="classif")],na.rm = TRUE),
                median_regr = median(median_width[which(task_type=="regr")]/sd[which(task_type=="regr")]^2,
                                     na.rm = TRUE)
              ), by = c("inducer", "outer_reps","inner_reps")]
  min_size = min(ylims)
  max_size = max(ylims)

  columns <- c("y_R","y_ER")

  data$method <- "conz"

  plotdat <- pivot_longer(data, cols = all_of(columns),
                          names_to = "coverage_of", values_to = "value")

  output <- ggplot(plotdat,aes(x=outer_reps,y=value,color=inducer))+
    geom_hline(yintercept = 0.95,color="black") +
    geom_line(aes(linetype=coverage_of),size=0.33) +
    facet_grid(method~inner_reps,scales="free_x", switch="x") +
    ylim(min_size,max_size) +
    theme_bw() +
    labs(color = "Inducer", linetype = "Coverage of",
         x="Inner repetitions\nOuter repetitions", y="Average coverage") +
    scale_color_brewer(palette = "Set1",
                       labels=c("decision_tree" = "Decision Tree",
                                "random_forest"="Random Forest",
                                "lm_or_logreg"="Linear or Logistic regression",
                                "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression"))+
    scale_linetype_discrete(labels = c("y_ER"="Expected Risk",
                                       "y_R"="Risk"))


  plotdat2 <- pivot_longer(data, cols = all_of(c("median_classif")),#,"median_regr")),
                           names_to = "aggr", values_to = "value")

  output2 <- ggplot(plotdat2,aes(x=outer_reps,y=value,fill=aggr))+
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(method~inner_reps,scales="free_x", switch="x") +
    scale_y_continuous(breaks=seq(0.1,0.2,by=0.1),expand = c(0,0)) +
   # geom_hline(yintercept = 0.2,color="grey33") +
    #geom_hline(yintercept = 0.4,color="grey33",linetype="dotted")+
    theme_classic() +
    labs(fill = "Median width for",y="") +
    scale_fill_manual(values=c("steelblue","slateblue"),
                      labels=c(
                        "median_classif" = "Classification",
                        "median_regr" = "Regression (relative to target's sample variance)"
                      ))

  return(list(output,output2))
}

aggr_plot_ncv <- function(data, inducers, DGPs, ylims=c(0,1), SDs){
  data <- merge(data,SDs,by="dgp")
  data = data[as.character(dgp) %in% DGPs,
              list(
                y_R = mean(cov_R),
                y_ER = mean(cov_ER),
                median_classif = median(median_width[which(task_type=="classif")],na.rm = TRUE),
                median_regr = median(median_width[which(task_type=="regr")]/sd[which(task_type=="regr")]^2,
                                     na.rm = TRUE)
              ), by = c("inducer", "reps_outer")]
  min_size = min(ylims)
  max_size = max(ylims)

  columns <- c("y_R","y_ER")

  data$method <- "ncv"

  plotdat <- pivot_longer(data, cols = all_of(columns),
                          names_to = "coverage_of", values_to = "value")

  output <- ggplot(plotdat,aes(x=reps_outer,y=value,color=inducer))+
    geom_hline(yintercept = 0.95,color="black") +
    geom_line(aes(linetype=coverage_of),size=0.33) +
    facet_grid(method~.,scales="free_x") +
    ylim(min_size,max_size) +
    theme_bw() +
    labs(color = "Inducer", linetype = "Coverage of",
         x="Outer repetitions", y="Average coverage") +
    scale_color_brewer(palette = "Set1",
                       labels=c("decision_tree"="Decision Tree",
                                "random_forest"="Random Forest",
                                "lm_or_logreg"="Linear or Logistic regression",
                                "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression"))+
    scale_linetype_discrete(labels = c("y_ER"="Expected Risk",
                                       "y_R"="Risk"))

  plotdat2 <- pivot_longer(data, cols = all_of(c("median_classif")),#,"median_regr")),
                           names_to = "aggr", values_to = "value")

  output2 <- ggplot(plotdat2,aes(x=reps_outer,y=value,fill=aggr))+
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_y_continuous(breaks=seq(0.1,0.2,by=0.1), limits=c(0,0.2), expand = c(0,0)) +
    #geom_hline(yintercept = 0.2,color="grey33") +
    #geom_hline(yintercept = 0.4,color="grey33",linetype="dotted")+
    facet_grid(method~.,scales="free_x") +
    theme_classic() +
    labs(fill = "Median width for",y="") +
    scale_fill_manual(values=c("steelblue","slateblue"),
                                        labels=c(
                                          "median_classif" = "Classification",
                                          "median_regr" = "Regression (relative to target's sample variance)"
                                        ))

  return(list(output,output2))
}


aggr_plot_ho <- function(data, inducers, DGPs, ylims=c(0,1), SDs){
  data <- merge(data,SDs,by="dgp")
  data = data[as.character(dgp) %in% DGPs &
              size!=100,
              list(
                y_R = mean(cov_R),
                y_ER = mean(cov_ER),
                median_classif = median(median_width[which(task_type=="classif")],na.rm = TRUE),
                median_regr = median(median_width[which(task_type=="regr")]/sd[which(task_type=="regr")]^2,
                                     na.rm = TRUE)
              ), by = c("inducer", "ratio")]
  min_size = min(ylims)
  max_size = max(ylims)

  columns <- c("y_R","y_ER")

  data$method <- "holdout"

  plotdat <- pivot_longer(data, cols = all_of(columns),
                          names_to = "coverage_of", values_to = "value")

  output <- ggplot(plotdat,aes(x=ratio,y=value,color=inducer))+
    geom_hline(yintercept = 0.95,color="black") +
    geom_line(aes(linetype=coverage_of),size=0.33) +
    facet_grid(method~.,scales="free_x") +
    ylim(min_size,max_size) +
    theme_bw() +
    labs(color = "Inducer", linetype = "Coverage of",
         x="Ratio", y="Average coverage") +
    scale_color_brewer(palette = "Set1",
                       labels=c("decision_tree"="Decision Tree",
                                "random_forest"="Random Forest",
                                "lm_or_logreg"="Linear or Logistic regression",
                                "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression"))+
    scale_linetype_discrete(labels = c("y_ER"="Expected Risk",
                                       "y_R"="Risk"))

  plotdat2 <- pivot_longer(data, cols = all_of(c("median_classif")),#,"median_regr")),
                           names_to = "aggr", values_to = "value")

  output2 <- ggplot(plotdat2,aes(x=ratio,y=value,fill=aggr))+
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_y_continuous(breaks=seq(0.025,0.1,by=0.025), limits=c(0,0.1) ,
                       expand = c(0,0)) +
    #geom_hline(yintercept = 0.2,color="grey33") +

    facet_grid(method~.,scales="free_x") +
    theme_classic() +
    labs(fill = "Median width for",y="") +
    scale_fill_manual(values=c("steelblue","slateblue"),
                      labels=c(
                        "median_classif" = "Classification",
                        "median_regr" = "Regression (relative to target's sample variance)"
                      ))

  return(list(output,output2))
}


aggr_plot_cv <- function(data, inducers, DGPs, ylims=c(0,1), SDs){
  data <- merge(data,SDs,by="dgp")
  data = data[as.character(dgp) %in% DGPs &
              size!=100,
              list(
                y_R = mean(cov_R),
                y_ER = mean(cov_ER),
                median_classif = median(median_width[which(task_type=="classif")],na.rm = TRUE),
                median_regr = median(median_width[which(task_type=="regr")]/sd[which(task_type=="regr")]^2,
                                     na.rm = TRUE)
              ), by = c("inducer", "folds")]
  min_size = min(ylims)
  max_size = max(ylims)

  columns <- c("y_R","y_ER")

  data$method <- "cv_allpairs"

  plotdat <- pivot_longer(data, cols = all_of(columns),
                          names_to = "coverage_of", values_to = "value")

  output <- ggplot(plotdat,aes(x=folds,y=value,color=inducer))+
    geom_hline(yintercept = 0.95,color="black") +
    geom_line(aes(linetype=coverage_of),size=0.33) +
    facet_grid(method~.,scales="free_x") +
    ylim(min_size,max_size) +
    theme_bw() +
    labs(color = "Inducer", linetype = "Coverage of",
         x="Folds", y="Average coverage") +
    scale_color_brewer(palette = "Set1",
                       labels=c("decision_tree"="Decision Tree",
                                "random_forest"="Random Forest",
                                "lm_or_logreg"="Linear or Logistic regression",
                                "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression"))+
    scale_linetype_discrete(labels = c("y_ER"="Expected Risk",
                                       "y_R"="Risk"))

  plotdat2 <- pivot_longer(data, cols = all_of(c("median_classif")),#,"median_regr")),
                           names_to = "aggr", values_to = "value")

  output2 <- ggplot(plotdat2,aes(x=folds,y=value,fill=aggr))+
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_y_continuous(breaks=seq(0.025,0.1,by=0.025), limits=c(0,0.1) ,
      ,expand = c(0,0)) +
    #geom_hline(yintercept = 0.2,color="grey33") +

    facet_grid(method~.,scales="free_x") +
    theme_classic() +
    labs(fill = "Median width for",y="") +
    scale_fill_manual(values=c("steelblue","slateblue"),
                      labels=c(
                        "median_classif" = "Classification",
                        "median_regr" = "Regression (relative to target's sample variance)"
                      ))

  return(list(output,output2))
}


aggr_plot_cort <- function(data, inducers, DGPs, ylims=c(0,1), SDs){
  data <- merge(data,SDs,by="dgp")
  data = data[as.character(dgp) %in% DGPs &
              size!=100,
              list(
                y_R = mean(cov_R),
                y_ER = mean(cov_ER),
                median_classif = median(median_width[which(task_type=="classif")],na.rm = TRUE),
                median_regr = median(median_width[which(task_type=="regr")]/sd[which(task_type=="regr")]^2,
                                     na.rm = TRUE)
              ), by = c("inducer", "reps","ratio")]
  min_size = min(ylims)
  max_size = max(ylims)

  columns <- c("y_R","y_ER")

  data$method <- "cort"

  plotdat <- pivot_longer(data, cols = all_of(columns),
                          names_to = "coverage_of", values_to = "value")

  output <- ggplot(plotdat,aes(x=reps,y=value,color=inducer))+
    geom_hline(yintercept = 0.95,color="black") +
    geom_line(aes(linetype=coverage_of),size=0.33) +
    facet_grid(method~ratio,scales="free_x", switch="x") +
    ylim(min_size,max_size) +
    theme_bw() +
    labs(color = "Inducer", linetype = "Coverage of",
         x="Ratio\nRepetitions", y="Average coverage") +
    scale_color_brewer(palette = "Set1",
                       labels=c("decision_tree"="Decision Tree",
                                "random_forest"="Random Forest",
                                "lm_or_logreg"="Linear or Logistic regression",
                                "ridge_lm_or_logreg"="Ridge-penalized Linear or Logistic regression"))+
    scale_linetype_discrete(labels = c("y_ER"="Expected Risk",
                                       "y_R"="Risk"))


  plotdat2 <- pivot_longer(data, cols = all_of(c("median_classif")),#,"median_regr")),
                           names_to = "aggr", values_to = "value")

  output2 <- ggplot(plotdat2,aes(x=reps,y=value,fill=aggr))+
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(method~ratio,scales="free_x", switch="x") +
    scale_y_continuous(breaks=seq(0.025,0.1,by=0.025), limits=c(0,0.1) ,
                       ,expand = c(0,0)) +
    #geom_hline(yintercept = 0.2,color="grey33") +
    theme_classic() +
    labs(fill = "Median width for",y="") +
    scale_fill_manual(values=c("steelblue","slateblue"),
                      labels=c(
                        "median_classif" = "Classification",
                        "median_regr" = "Regression (relative to target's sample variance)"
                      ))

  return(list(output,output2))
}



