specification_inducer_performance = function(id) {
  ns = NS(id)
  tabPanel(
    "Inducer Performance",
    div(
      class = "content",
      fluidRow(column(12,
        HTML(paste(readLines("HTMLS/inducer_performance.html"), collapse = "")))),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("dgp"), "DGP", DGPS, DGPS[[1]]),
              br(),
              actionButton("Vinducer_performance", "View Plot"),
              downloadButton(ns("Dinducer_performance"), "Download Plot")
            )
          )
        ),
        mainPanel(
          div(
            class = "plot-container",
            plotlyOutput(ns("Pinducer_performance"))
          )
        )
    ))
  )
}


make_inducer_performance_plot = function(data, input, globalOps) {


  data = data[
    method == "bayle_10_all_pairs" & 
      dgp == input$dgp & 
      measure %in% translate_losses(input$loss_regr, input$loss_classif)
    ]

  data[, let(
    lower = ER - R_sd / sqrt(500) * 1.96,
    upper = ER + R_sd / sqrt(500) * 1.96
  )]


  ggplot(data = data, aes(x = size, y = ER, color = inducer)) + 
    geom_line(aes(x = size, y = ER)) + 
    geom_errorbar(aes(x = size, ymin = lower, ymax = upper)) + 
    labs(
      y = "Expected Risk",
      x = "Dataset Size"
    )
 }
