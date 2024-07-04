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



make_baseplot_sus = function(plotdata, y, input_evaluation, input_range, inducers, scales, colors, breaks) {
  plotdata$method = paste0(plotdata$method, " (", plotdata$iters, ")")
  if (input_evaluation == "Coverage Frequency") {
    output = ggplot(plotdata, aes_string(x = y, y = "inducer", shape = "dataset_type", color = "stand_width", label = "numwidth")) +
      geom_vline(xintercept = 0.95, color = "darkred", alpha = 0.6) +
      geom_vline(xintercept = 0.5, color = "black", alpha = 0.6) +
      binned_scale(aesthetics = "color",
                   scale_name = "stepsn",
                   palette = function(x) colors,
                   breaks = breaks,
                   limits = c(min(breaks), max(breaks)),
                   show.limits = TRUE,
                   guide = "colorsteps"
      ) +
      geom_point() + geom_text(hjust = ifelse(plotdata[[y]] < input_range[1] + 0.1, -0.2, 1.2), vjust = 0, size = 2.5) +
      facet_wrap(as.formula(paste("~", "method+inducer")), scales = scales)
  } else {
    output = ggplot(plotdata, aes_string(x = y, y = "inducer", shape = "dataset_type", color = "stand_width", label = "numwidth")) +
      scale_colour_stepsn(colours = colors,
                          limits = c(min(breaks), max(breaks)),
                          guide = guide_coloursteps(even.steps = FALSE,
                                                    show.limits = TRUE),
                          breaks = breaks) +
      geom_point() + geom_text(hjust = ifelse(plotdata[[y]] < input_range[1] + 0.1, -0.2, 1.2), vjust = 0, size = 2.5)
  }
  return(output)
}

make_susplots = function(data,
                        input_y, input_range, input_sizes,
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
    data[size %in% input_sizes & measure %in% translate_losses(input_loss_regr, input_loss_classif),
         list(
           y_R = sqrt(mean((cov_R - 0.95)^2)),
           y_ER = sqrt(mean((cov_ER - 0.95)^2)),
           y_PQ = sqrt(mean((cov_PQ - 0.95)^2)),
           width = width,
           iters = iters
         ), by = vec]
  } else {
    data[size %in% input_sizes & measure %in% translate_losses(input_loss_regr, input_loss_classif),
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
  
  byprod = plotdata
  plotdata$inducer <- paste0(plotdata$inducer,"_size",plotdata$size)
  plotdata <- plotdata[which(plotdata$size%in%sizes)]
  
  p = make_baseplot_sus(plotdata, y, input_evaluation, input_range, inducers, scales, colors, breaks)
  
  
    if (!sep_reg_class) {
      output = p + facet_wrap(vars(method))
    } else {
      output = p + facet_grid(task_type ~ method)
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
  output_edit <- output + scale_x_continuous(limits=c(0,1))
  
  return(list(plot=output_edit,byprod = byprod))
}






workingitout <- make_susplots(data = ci_aggr,
             input_y = "Expected Risk", input_range = c(0, 1), input_sizes = c(500,1000,5000,10000),
             input_evaluation = "Coverage Frequency",
             input_loss_regr = "Squared", input_loss_classif = "Zero-One",
             methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), 
             dgps = c("adult"), inducers = INDUCERS,stand="mean")

workingitout$plot
ggsave("64plots/PNGs/susDGPs/simulated_adult.png", width = 12, height = 14)

workingitout2 <- make_susplots(data = ci_aggr,
                              input_y = "Expected Risk", input_range = c(0, 1), input_sizes = c(500,1000,5000,10000),
                              input_evaluation = "Coverage Frequency",
                              input_loss_regr = "Squared", input_loss_classif = "Zero-One",
                              methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), 
                              dgps = c("physiochemical_protein"), inducers = INDUCERS,stand="mean")

workingitout2$plot+ggtitle("physiochemical_protein: Expected Risk, loss: L2/0-1")
ggsave("64plots/PNGs/susDGPs/simulated_physiochemical_protein.png", width = 12, height = 14)


workingitout3 <- make_susplots(data = ci_aggr,
                               input_y = "Expected Risk", input_range = c(0, 1), input_sizes = c(500,1000,5000,10000),
                               input_evaluation = "Coverage Frequency",
                               input_loss_regr = "Squared", input_loss_classif = "Zero-One",
                               methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), 
                               dgps = c("video_transcoding"), inducers = INDUCERS,stand="mean")

workingitout3$plot
ggsave("64plots/PNGs/susDGPs/simulated_video_transcoding.png", width = 12, height = 14)



workingitoutR <- make_susplots(data = ci_aggr,
                              input_y = "Risk", input_range = c(0, 1), input_sizes = c(500,1000,5000,10000),
                              input_evaluation = "Coverage Frequency",
                              input_loss_regr = "Squared", input_loss_classif = "Zero-One",
                              methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), 
                              dgps = c("adult"), inducers = INDUCERS,stand="mean")

workingitoutR$plot
ggsave("64plots/PNGs/susDGPs/simulated_adult_Risk.png", width = 12, height = 14)

workingitoutR2 <- make_susplots(data = ci_aggr,
                               input_y = "Risk", input_range = c(0, 1), input_sizes = c(500,1000,5000,10000),
                               input_evaluation = "Coverage Frequency",
                               input_loss_regr = "Squared", input_loss_classif = "Zero-One",
                               methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), 
                               dgps = c("physiochemical_protein"), inducers = INDUCERS,stand="mean")

workingitoutR2$plot
ggsave("64plots/PNGs/susDGPs/simulated_physiochemical_protein_Risk.png", width = 12, height = 14)


workingitoutR3 <- make_susplots(data = ci_aggr,
                               input_y = "Risk", input_range = c(0, 1), input_sizes = c(500,1000,5000,10000),
                               input_evaluation = "Coverage Frequency",
                               input_loss_regr = "Squared", input_loss_classif = "Zero-One",
                               methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), 
                               dgps = c("video_transcoding"), inducers = INDUCERS,stand="mean")

workingitoutR3$plot
ggsave("64plots/PNGs/susDGPs/simulated_video_transcoding_Risk.png", width = 12, height = 14)







