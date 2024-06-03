library(dplyr)
library(ggplot2)
library(data.table)
library(here)
library(viridis)

source("shiny_app/setup.R")
source("64plots/helpers.R")


## Edited fallback_plot

make_64plots = function(data, 
                         input_y, input_range, input_size,
                         input_evaluation, 
                         input_loss_regr, input_loss_classif,
                         input_free_scales = "none",
                         methods, dgps, inducers,
                         sep_reg_class = FALSE) {
  data = data[as.character(method) %in% methods & as.character(dgp) %in% dgps & as.character(inducer) %in% inducers]
  y = switch(input_y,
             "Risk" = "y_R",    "Expected Risk" = "y_ER",
             "Proxy Quantity" = "y_PQ"
  )
  
  scales = translate_scales(input_free_scales)
  
  min_y = input_range[1]
  max_y = input_range[2]
  
  vec = c("inducer", "dgp", "method", "size", "width")
  
  newdat = if (input_evaluation == "Coverage Error") {
    data[size >= min_size & size <= max_size & measure %in% translate_losses(input_loss_regr, input_loss_classif),
         list(
           y_R = sqrt(mean((cov_R - 0.95)^2)),
           y_ER = sqrt(mean((cov_ER - 0.95)^2)),
           y_PQ = sqrt(mean((cov_PQ - 0.95)^2))
         ), by = vec]
  } else {
    data[size==input_size & measure %in% translate_losses(input_loss_regr, input_loss_classif),
         list(
           y_R = mean(cov_R),
           y_ER = mean(cov_ER),
           y_PQ = mean(cov_PQ)
         ), by = vec]
  }

  ####
  plotdata <- add_info(newdat,DATA_OVERVIEW,"dgp","name")
  plotdata$width <- round(plotdata$width,2)
  plotdata$numwidth <- as.character(plotdata$width)
  
  max <- round(max(plotdata$width),1)
  breaks <- c(0, 0.5, 1, 2, max)
  colors <- c("darkgreen","darkblue", "purple", "red")
  ####
  
  p <- make_baseplot(plotdata,y,input_evaluation,input_range,inducers,scales,colors,breaks)
  

  if(length(inducers)==1){
  if(!sep_reg_class){
  output <- p + facet_wrap(vars(method))
  } else {
  output <- p + facet_grid(task_type ~ method) 
  }
  } else {
  output <- p + facet_wrap(vars(method,inducer))
    }
  
  output = output +     
    labs(
    x = input_evaluation,
    y = "DGP",
    shape= "DGP type"
    ) +
    theme_bw() + 
    theme(legend.position = "bottom", legend.box="vertical") +
    guides(color = guide_colorbar(title = "Average CI width",
                                 barwidth = 30,
                                 barheight=0.5)) 
    
  return(output)
}

make_64plots(data=ci_aggr,
             input_y = "Expected Risk",input_range = c(0,1), input_size = 500,
             input_evaluation = "Coverage Frequency",
             input_loss_regr = "Squared", input_loss_classif = "Zero-One",
             methods = DEFAULT_METHODS, dgps = DGPS, inducers = "lm_or_logreg") 
ggsave("64plots/PNGs/ER_lmlog.png",width=9,height=7.5)

make_64plots(data=ci_aggr,
             input_y = "Expected Risk",input_range = c(0,1), input_size = 500,
             input_evaluation = "Coverage Frequency",
             input_loss_regr = "Squared", input_loss_classif = "Zero-One",
             methods = DEFAULT_METHODS, dgps = DGPS, inducers = "decision_tree") 
ggsave("64plots/PNGs/ER_decisiontree.png",width=9,height=7.5)

make_64plots(data=ci_aggr,
             input_y = "Risk",input_range = c(0,1), input_size = 500,
             input_evaluation = "Coverage Frequency",
             input_loss_regr = "Squared", input_loss_classif = "Zero-One",
             methods = DEFAULT_METHODS, dgps = DGPS, inducers = INDUCERS,
             sep_reg_class = FALSE) 



