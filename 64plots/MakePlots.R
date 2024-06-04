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
  plotdata[, let(
    stand_width = round(width / .SD[method == "bayle_10_all_pairs", "width"][[1L]],2)
  ), by = "dgp"]
  plotdata$numwidth = as.character(plotdata$stand_width)

  breaks = c(0, 0.5, 1, 1.5, max(plotdata$stand_width))
  colors = c("darkgreen", "darkblue", "purple", "red")
  ####


  breaks = c(0, 0.5, 1, 2, 3, max(plotdata$stand_width))
  colors = c("darkgreen", "darkblue", "orange", "purple", "red")
  ####

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

  output = output +
    labs(
      x = input_evaluation,
      y = "DGP",
      shape = "DGP type"
    ) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical") +
    guides(color = guide_colorbar(title = "CI width rel. to Bayle (10)",
      barwidth = 30,
      barheight = 0.5))

  return(output)
}

make_64plots(data = ci_aggr,
  input_y = "Expected Risk", input_range = c(0, 1), input_size = 500,
  input_evaluation = "Coverage Frequency",
  input_loss_regr = "Squared", input_loss_classif = "Zero-One",
  methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), dgps = DGPS, inducers = "lm_or_logreg") + ggtitle("Plot for expected risk, classic loss and lm/logistic regression")
ggsave("64plots/PNGs/ER_lmlog.png", width = 9, height = 14)

make_64plots(data = ci_aggr,
  input_y = "Expected Risk", input_range = c(0, 1), input_size = 500,
  input_evaluation = "Coverage Frequency",
  input_loss_regr = "Squared", input_loss_classif = "Zero-One",
  methods = setdiff(DEFAULT_METHODS, c("ls_bootstrap_100", "ts_bootstrap")), dgps = DGPS, inducers = "decision_tree") + ggtitle("Plot for expected risk, classic loss and decision tree")
ggsave("64plots/PNGs/ER_decisiontree.png", width = 9, height = 14)

make_64plots(data = ci_aggr,
  input_y = "Risk", input_range = c(0, 1), input_size = 500,
  input_evaluation = "Coverage Frequency",
  input_loss_regr = "Squared", input_loss_classif = "Zero-One",
  methods = DEFAULT_METHODS, dgps = DGPS, inducers = INDUCERS,
  sep_reg_class = FALSE)
make_64plots(data = ci_aggr,
  input_y = "Risk", input_range = c(0, 1), input_size = 100,
  input_evaluation = "Coverage Frequency",
  input_loss_regr = "Squared", input_loss_classif = "Zero-One",
  methods = DEFAULT_METHODS, dgps = DGPS, inducers = "random_forest",
  sep_reg_class = FALSE) + ggtitle(
  paste("inducer: ", "random_forest", ", size: ", 100, ", target: ", "Risk", ", loss: L2/0-1")
)

ggsave(here("64plots", "PNGs", "test.png"), width = 12, height = 14)

inducers = unique(ci_aggr$inducer)
sizes = unique(ci_aggr$size)

for (inducer in inducers) {
  for (size in sizes) {
    for (input_y in c("Risk", "Expected Risk")) {
      make_64plots(data = ci_aggr,
        input_y = input_y, input_range = c(0, 1), input_size = size,
        input_evaluation = "Coverage Frequency",
        input_loss_regr = "Squared", input_loss_classif = "Zero-One",
        methods = DEFAULT_METHODS, dgps = DGPS, inducers = inducer,
        sep_reg_class = FALSE) + ggtitle(
        paste("inducer: ", inducer, ", size: ", size, ", target: ", input_y, ", loss: L2/0-1")
      )
      ggsave(here("64plots", "PNGs", "classic_losses", paste0(inducer, "_", size, "_", gsub(" ", "", input_y), ".png")), width = 12, height = 14)
    }
  }
}

# not also for the different loss functions. Here we need to split by task type, otherwise we have 3 * 4 = 12 combinations


for (inducer in inducers) {
  for (size in sizes) {
    for (input_y in c("Risk", "Expected Risk")) {
      for (loss in unlist(LOSSES)) {
        loss_t = translate_losses(loss)
        make_64plots(data = ci_aggr,
          input_y = "Risk", input_range = c(0, 1), input_size = size,
          input_evaluation = "Coverage Frequency",
          input_loss_regr = loss, input_loss_classif = loss,
          methods = DEFAULT_METHODS, dgps = DGPS, inducers = inducer,
          sep_reg_class = FALSE) + ggtitle(
          paste("inducer: ", inducer, ", size: ", size, ", target: ", input_y, ", loss: ", loss)
        )
        ggsave(here("64plots", "PNGs", "other_losses", paste0(inducer, "_", size, "_", gsub(" ", "", input_y), "_", loss_t, ".png")), width = 12, height = 14)
      }
    }
  }
}
