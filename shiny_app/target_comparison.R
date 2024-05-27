# agreggated

specification_target_comparsion = function(id) {
  ns = NS(id)
  tabPanel(
    "Target Comparison",
    div(
      class = "content",
      fluidRow(column(12,
        HTML(paste(readLines("HTMLS/target_comparison.html"), collapse = "")))),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              sliderInput(ns("range"), "Range:", min = 0, max = 1, value = c(0, 1)),
              selectInput(ns("min_size"), "Min Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
              selectInput(ns("max_size"), "Max Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "10000"),
              selectInput(ns("free_scales"), "Free Scales:", choices = c("x", "y", "both", "none"), "none"),
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("evaluation"), "Evaluation:", choices = c("Coverage Error", "Coverage Frequency"), "Coverage Error"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("method"), "Method:", choices = PQ_METHODS, selected = "bayle_10_within"),
              pickerInput(ns("inducers"), "Inducers:",
                choices = INDUCERS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = INDUCERS),
              pickerInput(ns("dgps"), "DGPS:",
                choices = DGPS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = DGPS),
              br(),
              actionButton("Vtarget_comparison", "View Plot"),
              downloadButton(ns("Dtarget_comparison"), "Download Plot")
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
            plotlyOutput(ns("Ptarget_comparison"))
          )
        )
    ))
  )
}


make_target_comparison_plot = function(data, input, globalOps) {
  data = data[as.character(method) %in% globalOps$methods_global() & as.character(dgp) %in% globalOps$dgps_global()]
  scales = translate_scales(input$free_scales)
  data = if (input$evaluation == "Coverage Error") {
    data[
      !is.na(cov_PQ) &
        measure %in% translate_losses(input$loss_regr, input$loss_classif) &
        size >= as.integer(input$min_size) & size <= as.integer(input$max_size) &
        dgp %in% input$dgps &
        method == input$method &
        inducer %in% input$inducers,
        list(
          PQ = sqrt(mean((cov_PQ - 0.95)^2)),
          R  = sqrt(mean((cov_R - 0.95)^2)),
          ER  = sqrt(mean((cov_ER - 0.95)^2))
        ),
      by = c("size", "inducer")
    ]
  } else {
    data[
      !is.na(cov_PQ) &
        measure %in% translate_losses(input$loss_regr, input$loss_classif) &
        size >= as.integer(input$min_size) & size <= as.integer(input$max_size) &
        dgp %in% input$dgps &
        method == input$method &
        inducer %in% input$inducers,
        list(
          PQ = mean(cov_PQ),
          R  = mean(cov_R),
          ER  = mean(cov_ER)
        ),
      by = c("size", "inducer")
    ]
  }

  data = melt(data, id.vars = c("size", "inducer"), measure.vars = c("R", "ER", "PQ"),
    variable.name = "target", value.name = "coverage")

  p = ggplot(data, aes(x = size, y = coverage, color = target)) +
    facet_wrap(vars(inducer), scales = scales) +
    # geom_boxplot() +
    geom_line() +
    ylim(input$range[1], input$range[2]) +
    labs(
      y = input$evaluation,
      x = "Dataset Size"
    )

  if (input$evaluation == "Coverage Frequency") {
    p = p + geom_hline(yintercept = 0.95, color = "red")
  }

  p
}
