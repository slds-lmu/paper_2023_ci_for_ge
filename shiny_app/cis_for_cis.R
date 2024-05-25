specification_cis_for_cis = function(id) {
  ns = NS(id)
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
              selectInput(ns("inducer"), "inducer:", choices = INDUCERS),
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("dgp"), "DGPs:", choices = DGPS),
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


make_cis_for_cis_plot = function(data, input, globalOps) {
  data = data[
    inducer == input$inducer &
      measure %in% translate_losses(input$loss_regr, input$loss_classif) &
      dgp == input$dgp &
      size == input$size &
      method %in% input$methods,
  ]
  data[, let(method = droplevels(method))]
  cov_var = if (identical(input$target, "Risk")) "cov_R" else "cov_ER"
  data[, let(
    cov = get(cov_var),
    upper = get(cov_var) + 1.96 * get(paste0(cov_var, "_se")),
    lower = get(cov_var) - 1.96 * get(paste0(cov_var, "_se"))
  )]

  ggplot(data) +
    geom_point(aes(y = method, x = cov)) +
    geom_errorbar(aes(y = method, xmin = lower, xmax = upper))
}
