# agreggated

specification_width_vs_coverage = function(id) {
  ns = NS(id)
  tabPanel(
    "Width vs Coverage",
    div(
      class = "content",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              selectInput(ns("size"), "Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
              selectInput(ns("loss_regr"), "Loss(Regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss(Classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("target"), "Target:", choices = c("Risk", "Expected Risk"), "Risk"),
              selectInput(ns("sep_group"), "Separately show:", choices = c("task", "learner", "none")),
              pickerInput(ns("learners"), "Learners:",
                choices = LEARNERS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = LEARNERS),
              pickerInput(ns("methods"), "Methods:",
                choices = METHODS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = METHODS),
              pickerInput(ns("tasks"), "Tasks:",
                choices = TASKS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = TASKS),
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


make_width_vs_coverage_plot = function(data, input) {
  data = data[measure %in% translate_losses(input$loss_regr, input$loss_classif)]
  target = if (identical(input$target, "Risk")) "R" else "ER"

  # do this at the beginning, when bayle_10_all_pairs is always available
  data = data[, list(
    rel_width = width / .SD[method == "bayle_10_all_pairs", "width"][[1L]],
    method = method,
    cov = get(paste0("cov_", target))
  ), by = c("task", "size", "learner")]

  # ensure that only those are contained, which have all values
  data = data[size == as.integer(input$size), ]

  by_vars = c("method", "size")

  if (input$sep_group != "none") {
    by_vars = c(by_vars, input$sep_group)
  }

  data = data[
    learner %in% input$learners &
      task %in% input$tasks &
      method %in% input$methods,
    list(
      cov = mean(cov),
      rel_width = mean(rel_width)
    ), by = c(by_vars)]



  p = ggplot(data, aes(x = rel_width, y = cov, color = method)) +
    geom_point() +
    facet_wrap(vars(size), scales = "free_x")

  if (input$sep_group != "none") {
    p = p + facet_wrap(as.formula(paste0("~", input$sep_group)), scales = "free_x")
  }

  p +
    geom_hline(yintercept = 0.95, color = "red") +
    ylim(NA, 1) +
    labs(
      x = "Width relative to Bayle (10-folds, all-pairs)",
      y = paste0("Coverage of ", input$target)
    )
}
