specification_chen_10_null = function(id) {
  ns = NS(id)
  tabPanel(
    "Chen 10 Null",
    div(
      class = "content",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              selectInput(ns("min_size"), "Min Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
              selectInput(ns("max_size"), "Max Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "10000"),
              selectInput(ns("loss_regr"), "Loss(Regr):", LOSSES$regr, "Squared"),
              selectInput(ns("target"), "Target:", choices = c("Risk", "Expected Risk"), "Risk"),
              selectInput(ns("loss_classif"), "Loss(Classif):", LOSSES$classif, "Zero-One"),
              pickerInput(ns("methods"), "Methods:",
                choices = METHODS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = METHODS
              ),
              pickerInput(ns("learners"), "Learners:",
                choices = LEARNERS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = LEARNERS),
              br(),
              actionButton("Vchen_10_null", "View Plot"),
              downloadButton(ns("Dchen_10_null"), "Download Plot")
            )
          )
        ),
        mainPanel(
          div(
            class = "plot-container",
            plotlyOutput(ns("Pchen_10_null"))
          )
        )
    ))
  )
}


make_chen_10_null_plot = function(data, input) {
  target = translate_target(input$target)

  data = data[
    size >= as.integer(input$min_size) &
      size <= as.integer(input$max_size) & 
      measure %in% translate_losses(input$loss_regr, input$loss_classif) &
      method %in% input$methods &
      learner %in% input$learners,
      list(
        cov = mean(get(paste0("cov_", target)))
    ),
    by = c("learner", "size", "method")
  ]

  print(data)

  ggplot(data, aes(y = method, x = cov)) +
    facet_grid(vars(learner), vars(size)) +
    geom_point() + 
    geom_vline(xintercept = 0.95, color = "red") +
    xlim(NA, 1)

  }
