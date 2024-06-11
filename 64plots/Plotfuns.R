## Edited fallback_plot

make_64plots = function(data,
                        input_y, input_range, input_size,
                        input_evaluation,
                        input_loss_regr, input_loss_classif,
                        input_free_scales = "none",
                        methods, dgps, inducers,
                        sep_reg_class = FALSE,
                        stand="Bayle",
                        cutoff=0.5,
                        cutoff_method_check=c("bayle_10_all_pairs","corrected_t_10")){
  data = data[as.character(method) %in% methods & as.character(dgp) %in% dgps & as.character(inducer) %in% inducers]
  y = switch(input_y,
             "Risk" = "y_R", "Expected Risk" = "y_ER",
             "Proxy Quantity" = "y_PQ"
  )
  
  scales = translate_scales(input_free_scales)
  
  vec = c("inducer", "dgp", "method", "size")
  
  newdat = if (input_evaluation == "Coverage Error") {
    data[size == input_size & measure %in% translate_losses(input_loss_regr, input_loss_classif),
         list(
           y_R = sqrt(mean((cov_R - 0.95)^2)),
           y_ER = sqrt(mean((cov_ER - 0.95)^2)),
           y_PQ = sqrt(mean((cov_PQ - 0.95)^2)),
           width = width,
           iters = iters
         ), by = vec]
  } else {
    data[size == input_size & measure %in% translate_losses(input_loss_regr, input_loss_classif),
         list(
           y_R = mean(cov_R),
           y_ER = mean(cov_ER),
           y_PQ = mean(cov_PQ),
           width = width,
           iters = iters
         ), by = vec]
  }
  
  plotdata = newdat[DATA_OVERVIEW, on = c(dgp = "name_short"), nomatch = 0]
  if(stand == "Bayle"){
    plotdata[, let(
      stand_width = width / .SD[method == "bayle_10_all_pairs", "width"][[1L]]
    ), by = "dgp"]
    
    breaks = c(0, 0.5, 1, 2, 3, round(max(plotdata$stand_width),1))
    colors = c("darkgreen", "darkblue", "orange", "purple", "red")
  }else{
    plotdata[, let(
      stand_width = width / mean(width)
    ), by = "dgp"]
    breaks = round(quantile(plotdata$stand_width,c(0,0.5,0.9,1)),0)
    colors = c("darkgreen", "darkblue", "orange", "purple", "red")
  }
  
  plotdata$numwidth = as.character(round(plotdata$stand_width,2))
  
  #breaks = c(0, 0.5, 1, 1.5, max(plotdata$stand_width))
  #colors = c("darkgreen", "darkblue", "purple", "red")
  ####
  
  infodat <- plotdata %>% 
    group_by(method) %>% 
    summarize(avg_stand_width = mean(stand_width),
              under_cov_R = over_under(y_R,"under",0.05),
              under_cov_ER = over_under(y_ER,"under",0.05),
              over_cov_R = over_under(y_R,"over",0.05),
              over_cov_ER = over_under(y_ER,"over",0.05),
              n_under_R = over_under_cutoff(y_R,"under",0.05,cutoff)[["n"]],
              n_under_ER = over_under_cutoff(y_ER,"under",0.05,cutoff)[["n"]],
              cutoff_under_cov_R = over_under_cutoff(y_R,"under",0.05,cutoff)[["value"]],
              cutoff_under_cov_ER = over_under_cutoff(y_ER,"under",0.05,cutoff)[["value"]],
              n_over_R = over_under_cutoff(y_R,"over",0.05,cutoff)[["n"]],
              n_over_ER = over_under_cutoff(y_ER,"over",0.05,cutoff)[["n"]],
              cutoff_over_cov_R = over_under_cutoff(y_R,"over",0.05,cutoff)[["value"]],
              cutoff_over_cov_ER = over_under_cutoff(y_ER,"over",0.05,cutoff)[["value"]])
  
  
  suspicious_DGPS <- find_suspicious_dgps(plotdata,y,cutoff,cutoff_method_check,CI_width=10000)
  
  p = make_baseplot(plotdata, y, input_evaluation, input_range, inducers, scales, colors, breaks)
  
  
  if (length(inducers) == 1) {
    if (!sep_reg_class) {
      output = p + facet_wrap(vars(method))
    } else {
      output = p + facet_grid(task_type ~ method)
    }
  } else {
    output = p + facet_wrap(vars(method, inducer))
  }
  
  title=ifelse(stand == "Bayle","CI width rel. to Bayle (10)","CI width/average per DGP (quantiles)")
  output = output +
    labs(
      x = input_evaluation,
      y = "DGP",
      shape = "DGP type"
    ) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical") +
    guides(color = guide_colorbar(title = title,
                                  barwidth = 30,
                                  barheight = 0.5
    ))
  
  return(list(plot=output,info=infodat,suspicious_DGPs=suspicious_DGPS))
}

#make_64plots(data = ci_aggr,
             #input_y = "Expected Risk", input_range = c(0, 1), input_size = 500,
             #input_evaluation = "Coverage Frequency",
             #input_loss_regr = "Squared", input_loss_classif = "Zero-One",
             #methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), dgps = DGPS, inducers = "lm_or_logreg",stand="mean")[["plot"]] + ggtitle("Plot for expected risk, classic loss and lm/logistic regression")

#make_64plots(data = ci_aggr,
             #input_y = "Expected Risk", input_range = c(0, 1), input_size = 500,
             #input_evaluation = "Coverage Frequency",
             #input_loss_regr = "Squared", input_loss_classif = "Zero-One",
             #methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), dgps = DGPS, inducers = "decision_tree",stand="mean")[["plot"]] + ggtitle("Plot for expected risk, classic loss and decision tree")

#make_64plots(data = ci_aggr,
             #input_y = "Risk", input_range = c(0, 1), input_size = 500,
             #input_evaluation = "Coverage Frequency",
             #input_loss_regr = "Squared", input_loss_classif = "Zero-One",
             #methods = DEFAULT_METHODS, dgps = DGPS, inducers = INDUCERS,
             #sep_reg_class = FALSE)[["plot"]]
#make_64plots(data = ci_aggr,
             #input_y = "Risk", input_range = c(0, 1), input_size = 100,
             #input_evaluation = "Coverage Frequency",
             #input_loss_regr = "Squared", input_loss_classif = "Zero-One",
             #methods = DEFAULT_METHODS, dgps = DGPS, inducers = "random_forest",
             #sep_reg_class = FALSE)[["plot"]] + ggtitle(
             #paste("inducer: ", "random_forest", ", size: ", 100, ", target: ", "Risk", ", loss: L2/0-1")
             #)
