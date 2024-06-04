add_info = function(data, info, common_column, common_column_info = NULL) {
  result = info[data, on = list(dgp = "name_short")]
}


make_baseplot = function(plotdata, y, input_evaluation, input_range, inducers, scales, colors, breaks) {
  plotdata$method = paste0(plotdata$method, " (", plotdata$iters, ")")
  if (input_evaluation == "Coverage Frequency") {
    output = ggplot(plotdata, aes_string(x = y, y = "dgp", shape = "dataset_type", color = "stand_width", label = "numwidth")) +
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
  } else {
    output = ggplot(plotdata, aes_string(x = y, y = "dgp", shape = "dataset_type", color = "stand_width", label = "numwidth")) +
      scale_colour_stepsn(colours = colors,
        limits = c(min(breaks), max(breaks)),
        guide = guide_coloursteps(even.steps = FALSE,
          show.limits = TRUE),
        breaks = breaks) +
      geom_point() + geom_text(hjust = ifelse(plotdata[[y]] < input_range[1] + 0.1, -0.2, 1.2), vjust = 0, size = 2.5)
  }
  return(output)
}
