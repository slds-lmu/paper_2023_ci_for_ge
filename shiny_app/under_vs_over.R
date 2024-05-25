specification_under_vs_over = function(id) {
  ns = NS(id)
  tabPanel(
    "Under vs. Overcoverage",
    div(
      class = "content",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("target"), "Target:", choices = c("Risk", "Expected Risk"), "Risk"),
              selectInput(ns("dgp"), "DGP:", choices = DGPS),
              selectInput(ns("inducer"), "Inducer:", choices = INDUCERS),
              pickerInput(ns("methods"), "Methods:",
                choices = METHODS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = METHODS
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
            plotlyOutput(ns("Punder_vs_over"))
          )
        )
    ))
  )
}


make_under_vs_over_plot = function(data, input, globalOps) {
  target = translate_target(input$target)
  data = data[
      measure %in% translate_losses(input$loss_regr, input$loss_classif) &
      dgp == input$dgp &
      inducer == input$inducer &
      method %in% input$methods,
      list(
        under = mean(get(paste0("under_", target))),
        over = 1 - mean(get(paste0("cov_", target))) - mean(get(paste0("under_", target)))
      ),
    by = c("size", "method")
  ]

  data = melt(data, id.vars = c("size", "method"), measure.vars = c("over", "under"),
    variable.name = "type", value.name = "miss")

  ggplot(data, aes(y= method, x = miss, fill = type)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(vars(size)) + 
    geom_vline(xintercept = 0.05, color = "black")
}
