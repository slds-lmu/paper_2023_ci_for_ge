# agreggated

specification_width_vs_coverage <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Width vs Coverage",
    div(
      class = "content",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,
            selectInput(ns("size"), "Size Category", c("100", "[100, 500]", "[100, ..., 10k]"), "100"),
            selectInput(ns("target"), "Target:", choices = c("Risk", "Expected Risk"), "Risk"),
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
   target = if (identical(input$target, "Risk")) "R" else "ER"

  # do this at the beginning, when bayle_10_all_pairs is always available
  data = data[, list(
    rel_width = width / .SD[method == "bayle_10_all_pairs", "width"][[1L]],
    method = method,
    cov = get(paste0("cov_", target))
  ), by = c("task", "size", "learner")]

  # ensure that only those are contained, which have all values 
  data =  if (input$size == "100") {
    data[size == 100, ]
  } else if (input$size == "[100, 500]") {
    data[size <= 500 & !startsWith(as.character(method), "bccv")]
  } else {
    data[method %in% CHEAP_METHODS, ]
  }

  by_vars = c("method", "size")

 
  data = data[
    learner %in% input$learners & 
      task %in% input$tasks & 
      method %in% input$methods,
      list(
        cov = mean(cov),
        rel_width = mean(rel_width)
    ), by = c("method", "size")]



  p = ggplot(data, aes(x = rel_width, y = cov, color = method)) + 
    geom_point() + 
    facet_wrap(vars(size), scales = "free_x")

  # if (input$group != "none") {
  #   p <- p + facet_wrap(as.formula(paste0("~", input$group)), scales = "free_x")
  # }

  p + 
    geom_hline(yintercept = 0.95, color = "red") + 
    ylim(NA, 1) + 
    labs(
      x = "Width relative to Bayle (10-folds, all-pairs)",
      y = paste0("Coverage of ", input$target)
    )
}


