# agreggated

specification_width_vs_coverage = function(id) {
  ns = NS(id)
  tabPanel(
    "Width vs Coverage Error",
    div(
      class = "content",
      fluidRow(column(12,
        HTML(paste(readLines("HTMLS/width_vs_coverage.html"), collapse = "")))),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              selectInput(ns("size"), "Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
              selectInput(ns("free_scales"), "Free Scales:", choices = c("x", "y", "both", "none"), "none"),
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("target"), "Target:", choices = c("Risk", "Expected Risk"), "Risk"),
              selectInput(ns("sep_group"), "Group:", choices = c("dgp", "inducer", "none")),
              pickerInput(ns("inducers"), "inducers:",
                choices = INDUCERS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = INDUCERS),
              pickerInput(ns("methods"), "Methods:",
                choices = METHODS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = METHODS),
              pickerInput(ns("dgps"), "DGPs:",
                choices = DGPS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = DGPS),
              br(),
              actionButton("Vwidth_vs_coverage", "View Plot"),
              downloadButton(ns("Dwidth_vs_coverage"), "Download Plot")
            )
          )
        ),
        mainPanel(
          div(
            class = "plot-container",
            plotlyOutput(ns("Pwidth_vs_coverage"))
          )
        )
    ))
  )
}


make_width_vs_coverage_plot = function(data, input, globalOps) {
  data = data[measure %in% translate_losses(input$loss_regr, input$loss_classif)]
  target = if (identical(input$target, "Risk")) "R" else "ER"

  scales = translate_scales(input$free_scales)

  # do this at the beginning, when bayle_10_all_pairs is always available
  data = data[, list(
    rel_width = width / .SD[method == "bayle_10_all_pairs", "width"][[1L]],
    method = method,
    cov = get(paste0("cov_", target))
  ), by = c("dgp", "size", "inducer")]

  # ensure that only those are contained, which have all values
  data = data[size == as.integer(input$size), ]

  by_vars = c("method", "size")

  if (input$sep_group != "none") {
    by_vars = c(by_vars, input$sep_group)
  }

  data = data[
    inducer %in% input$inducers &
      dgp %in% input$dgps &
      method %in% input$methods,
    list(
      cov_error = sqrt(mean((cov - 0.95)^2)),
      rel_width = mean(rel_width)
    ), by = c(by_vars)]



  p = ggplot(data, aes(x = rel_width, y = cov_error, color = method)) +
    geom_point() +
    facet_wrap(vars(size), scales = scales)

  if (input$sep_group != "none") {
    p = if (input$free_scales == "none") {
      p + facet_wrap(as.formula(paste0("~", input$sep_group)))
    } else {
      scales = translate_scales(input$free_scales)
      p + facet_wrap(as.formula(paste0("~", input$sep_group)), scales = scales)
    }
  }

  p +
    labs(
      x = "Width relative to Bayle (10-folds, all-pairs)",
      y = paste0("Coverage Error (RMSE) for", input$target)
    )
}
