translate_scales = function(free_scales) {
  switch(free_scales,
    both = "free",
    x = "free_x",
    y = "free_y",
    NULL
  )
}

translate_loss <- function(loss) {
  switch(as.character(loss),
         "Squared" = "se",
         "Absolute" = "ae",
         "Perc. Sq." = "percentual_se",
         "Std. Sq." = "standardized_se",
         "Zero-One" = "zero_one",
         "Brier" = "bbrier",
         "Log-Loss" = "logloss"
  )
}

translate_losses <- function(...) {
  sapply(list(...), translate_loss)
}

make_baseplot = function(plotdata, y, inducers, scales, colors, breaks) {
  input_range = c(0, 1)
  plotdata$method = paste0(plotdata$method, " (", plotdata$iters, ")")
  ggplot(plotdata, aes_string(x = y, y = "dgp", color = "stand_width", label = "numwidth")) +
    geom_vline(xintercept = 0.95, color = "darkred", alpha = 0.6) +
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
}

make_64plots = function(data,
                        input_y, input_range, input_size,
                        input_loss_regr, input_loss_classif,
                        input_free_scales = "none",
                        methods, dgps, inducers,
                        cutoff=0.5){
  data = data[as.character(method) %in% methods & as.character(dgp) %in% dgps & as.character(inducer) %in% inducers]
  y = switch(as.character(input_y),
             "Risk" = "y_R", "Expected Risk" = "y_ER",
             "Proxy Quantity" = "y_PQ"
  )

  scales = translate_scales(input_free_scales)

  vec = c("inducer", "dgp", "method", "size")

  newdat = data[size == input_size & loss %in% c(input_loss_regr, input_loss_classif),
         list(
           y_R = mean(cov_R),
           y_ER = mean(cov_ER),
           y_PQ = mean(cov_PQ),
           width = width,
           iters = iters
         ), by = vec]

  plotdata = newdat

  plotdata[, let(
    stand_width = width / .SD[method == "cv_10_allpairs", "width"][[1L]]
  ), by = "dgp"]

  breaks = c(0, 0.5, 1, 2, 3, round(max(plotdata$stand_width),1))
  colors = c("darkgreen", "darkblue", "orange", "purple", "red")

  plotdata$numwidth = as.character(round(plotdata$stand_width,2))

  p = make_baseplot(plotdata, y, input_range, inducers, scales, colors, breaks)


  if (length(inducers) == 1) {
    output = p + facet_wrap(vars(method))
  } else {
    output = p + facet_wrap(vars(method, inducer))
  }

  output = output +
    labs(
      x = "Relative Coverage Frequency",
      y = "DGP",
      shape = "DGP type"
    ) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical") +
    guides(color = guide_colorbar(title = "CI width/average per DGP (quantiles)",
                                  barwidth = 30,
                                  barheight = 0.5
    ))

  output
}
