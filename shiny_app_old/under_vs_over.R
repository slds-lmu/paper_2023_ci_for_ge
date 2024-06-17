specification_under_vs_over = function(id) {
  ns = NS(id)
  tabPanel(
    "Under/Over-Cov",
    div(
      class = "content",
      fluidRow(column(12,
        HTML(paste(readLines("HTMLS/under_vs_over.html"), collapse = "")))),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("size"), "Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "500"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("free_scales"), "Free Scales:", choices = c("x", "none"), "none"),
              selectInput(ns("target"), "Target:", choices = c("Risk", "Expected Risk"), "Risk"),
              selectInput(ns("inducer"), "Inducer:", choices = INDUCERS),
              pickerInput(ns("methods"), "Methods:",
                choices = METHODS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = METHODS
              ),
              pickerInput(ns("dgps"), "DGPs:",
                choices = DGPS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = DGPS
              ),
              br(),
              actionButton("Vunder_vs_over", "View Plot"),
              downloadButton(ns("Dunder_vs_over"), "Download Plot")
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
            plotlyOutput(ns("Punder_vs_over"))
          )
        )
    ))
  )
}


make_under_vs_over_plot = function(data, input, globalOps) {
  scales = translate_scales(input$free_scales)
  target = translate_target(input$target)
  data = data[
      method %in% input$methods & 
      dgp %in% input$dgps & 
      measure %in% translate_losses(input$loss_regr, input$loss_classif) &
      size == as.integer(input$size) & 
      inducer == input$inducer,
      list(
        under = mean(get(paste0("under_", target))),
        over = 1 - mean(get(paste0("cov_", target))) - mean(get(paste0("under_", target)))
      ),
    by = c("method", "dgp")
  ]

  data = melt(data, id.vars = c("dgp", "method"), measure.vars = c("over", "under"),
    variable.name = "type", value.name = "miss")

  data.table::setnames(data, "type", "ci_position")
  data$ci_position = ifelse(data$ci_position == "under", "under_truth", "above_truth")

  ggplot(data, aes(y= method, x = miss, fill = ci_position)) +
    facet_wrap(vars(dgp), scales = scales) + 
    geom_bar(stat = "identity", position = "stack") +
    geom_vline(xintercept = 0.05, color = "black", linetype = "dotted") + 
    labs(
      x = "Relative Miscoverage Ratio",
      y = "Inference Method"
    )
}
