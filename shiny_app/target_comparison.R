# agreggated

specification_target_comparsion = function(id) {
  ns = NS(id)
  tabPanel(
    "Target Comparison",
    div(
      class = "content",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              selectInput(ns("min_size"), "Min Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
              selectInput(ns("max_size"), "Max Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "10000"),
              selectInput(ns("method"), "Method", choices = PQ_METHODS, selected = "bayle_10_within"),
              pickerInput(ns("learners"), "Learners:",
                choices = LEARNERS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = LEARNERS),
              pickerInput(ns("tasks"), "Tasks:",
                choices = TASKS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = TASKS),
              br(),
              actionButton("Vtarget_comparison", "View Plot"),
              downloadButton(ns("Dtarget_comparison"), "Download Plot")
            )
          )
        ),
        mainPanel(
          div(
            class = "plot-container",
            plotlyOutput(ns("Ptarget_comparison"))
          )
        )
    ))
  )
}


make_target_comparison_plot = function(data, input) {
  data = data[
    !is.na(cov_PQ) &
      size >= as.integer(input$min_size) & size <= as.integer(input$max_size) &
      task %in% input$tasks &
      method == input$method &
      learner %in% input$learners,
    list(cov_PQ = mean(cov_PQ), cov_R = mean(cov_R), cov_ER = mean(cov_ER)),
    by = c("size", "learner")
  ]

  data = melt(data, id.vars = c("size", "learner"), measure.vars = c("cov_R", "cov_ER", "cov_PQ"),
    variable.name = "target", value.name = "coverage")

  ggplot(data, aes(x = size, y = coverage, color = target)) +
    facet_wrap(vars(learner)) +
    geom_hline(color = "red", yintercept = 0.95) +
    ylim(NA, 1) +
    geom_line()
}
