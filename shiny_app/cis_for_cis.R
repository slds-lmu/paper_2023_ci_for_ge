specification_cis_for_cis <- function(id) {
  ns <- NS(id)
  tabPanel(
    "CIs for CIs",
    div(
      class = "content",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(
              6,
              selectInput(ns("size"), "Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
              selectInput(ns("learner"), "Learner:", choices = LEARNERS),
              selectInput(ns("task"), "Task:", choices = TASKS),
              selectInput(ns("target"), "Target:", choices = c("Risk", "Expected Risk"), "Risk"),
              pickerInput(ns("methods"), "Methods:",
                choices = METHODS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = METHODS
              ),
              br(),
              actionButton("Vcis_for_cis", "View Plot"),
              downloadButton(ns("Dcis_cor_cs"), "Download Plot")
            )
          )
        ),
        mainPanel(
          div(
            class = "plot-container",
            plotlyOutput(ns("Pcis_for_cis"))
          )
        )
      )
    )
  )
}


make_cis_for_cis_plot <- function(data, input) {
  data <- data[
    learner == input$learner &
      task == input$task &
      size == input$size &
      method %in% input$methods,
  ]
  data[, let(method = droplevels(method))]
  cov_var <- if (identical(input$target, "Risk")) "cov_R" else "cov_ER"
  data[, let(
    cov = get(cov_var),
    upper = get(cov_var) + 1.96 * get(paste0(cov_var, "_se")),
    lower = get(cov_var) - 1.96 * get(paste0(cov_var, "_se"))
  )]

  ggplot(data) +
    geom_point(aes(y = method, x = cov, color = method)) +
    geom_errorbar(aes(y = method, xmin = lower, xmax = upper, color = method))
}
