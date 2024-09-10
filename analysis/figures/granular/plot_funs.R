translate_scales = function(free_scales) {
  switch(free_scales,
    both = "free",
    x = "free_x",
    y = "free_y",
    NULL
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
      y_R = cov_R,
      y_ER = cov_ER,
      stand_width = width / estimate_sd,
      dgp = dgp,
      inducer = inducer,
      method = method,
      size = size,
      iters = iters
    )]

  plotdata = newdat
  newdat$stand_width = min(newdat$stand_width, 16)

  breaks = c(0, 3.5, 4.5, 6, 8, 16)
  colors = c("darkblue", "darkgreen", "orange", "purple", "red")

  plotdata$numwidth = as.character(round(plotdata$stand_width,2))

  p = make_baseplot(plotdata, y, inducers, scales, colors, breaks)


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
    guides(color = guide_colorbar(title = "CI width standardized by SD(estimate)\nColorization is clipped at 16.",
                                  barwidth = 30,
                                  barheight = 0.5
    ))

  output
}
