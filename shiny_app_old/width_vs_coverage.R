# agreggated

specification_width_vs_coverage = function(id) {
  ns = NS(id)
  tabPanel(
    "Width vs Coverage",
    div(
      class = "content",
      fluidRow(column(12,
        HTML(paste(readLines("HTMLS/width_vs_coverage.html"), collapse = "")))),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              # sliderInput(ns("range_width"), "Range (width):", min = 0, max = Inf, value = c(0, Inf)),
              # sliderInput(ns("range_cov"), "Range (cov):", min = 0, max = 1, value = c(0, 1)),
              selectInput(ns("size"), "Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "500"),
              selectInput(ns("free_scales"), "Free Scales:", choices = c("x", "y", "both", "none"), "x"),
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("target"), "Target:", choices = c("Risk", "Expected Risk", "Proxy Quantity"), "Risk"),
              selectInput(ns("evaluation"), "Evaluation:", choices = c("Coverage Error", "Coverage Frequency"), "Coverage Frequency"),
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
            numericInput(
              inputId = ns("height_input"),
              label = "Add to display height:",
              value = 400,    # Default value
              min = NA,     # Minimum value (optional)
              max = NA,     # Maximum value (optional)
              step = 50     # Step size (optional)
            ),
            plotlyOutput(ns("Pwidth_vs_coverage"))
          )
        )
    ))
  )
}


make_width_vs_coverage_plot = function(data, input, globalOps) {
  data = data[
    measure %in% translate_losses(input$loss_regr, input$loss_classif) & 
    as.character(method) %in% globalOps$methods_global() & 
    as.character(dgp) %in% globalOps$dgps_global()
  ]
  target = translate_target(input$target)

  scales = translate_scales(input$free_scales)
  if (target == "PQ") {
    data = data[!is.na(cov_PQ)]
    data$method = droplevels(data$method)
  }

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

  data = if (input$evaluation == "Coverage Error") {
   data[
    inducer %in% input$inducers &
      dgp %in% input$dgps &
      method %in% input$methods,
    list(
      cov_error = sqrt(mean((cov - 0.95)^2)),
      rel_width = mean(rel_width)
    ), by = c(by_vars)]
  } else {
   data[
    inducer %in% input$inducers &
      dgp %in% input$dgps &
      method %in% input$methods,
    list(
      cov_error = mean(cov),
      rel_width = mean(rel_width)
    ), by = c(by_vars)]
    
  }

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

  p = p +
    labs(
      x = "Width relative to Bayle (10-folds, all-pairs)",
      y = input$evaluation
    )

  if (input$evaluation == "Coverage Frequency") {
    p = p + geom_hline(yintercept = 0.95, color = "red")
  }
  p
}
