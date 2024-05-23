specification_learner_performance = function(id) {
  ns = NS(id)
  tabPanel(
    "Learner Performance",
    div(
      class = "content",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              selectInput(ns("loss_regr"), "Loss(Regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss(Classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("task"), "DGP", TASKS, TASKS[[1]]),
              br(),
              actionButton("Vlearner_performance", "View Plot"),
              downloadButton(ns("Dlearner_performance"), "Download Plot")
            )
          )
        ),
        mainPanel(
          div(
            class = "plot-container",
            plotlyOutput(ns("Plearner_performance"))
          )
        )
    ))
  )
}


make_learner_performance_plot = function(data, input) {

  data = data[
    method == "bayle_10_all_pairs" & 
      task == input$task & 
      measure %in% translate_losses(input$loss_regr, input$loss_classif)
    ]
saveRDS(data, "~/scratch/data.rds")


  data[, let(
    lower = ER - R_sd / sqrt(500) * 1.96,
    upper = ER + R_sd / sqrt(500) * 1.96,
    size = as.factor(size)
  )]


  ggplot(data = data, aes_string(y = "learner", x = "ER")) + 
    geom_point() +  
    geom_errorbarh(aes(xmin = lower, xmax = upper)) +
    facet_wrap(vars(size), scales = "free") + 
    labs(
      y = "Expected Risk"
    )
 }
