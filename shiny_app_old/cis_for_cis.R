specification_cis_for_cis = function(id) {
  ns = NS(id)
  tabPanel(
    "CIs for Coverage",
    div(
      class = "content",
      fluidRow(column(12,
        HTML(paste(readLines("HTMLS/cis_for_cis.html"), collapse = "")))),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(
              6,
              sliderInput(ns("range"), "Range:", min = 0, max = 1, value = c(0, 1)),
              selectInput(ns("size"), "Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
              selectInput(ns("inducer"), "inducer:", choices = INDUCERS),
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("dgp"), "DGP:", choices = DGPS),
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
            numericInput(
              inputId = ns("height_input"),
              label = "Add to display height:",
              value = 400,    # Default value
              min = NA,     # Minimum value (optional)
              max = NA,     # Maximum value (optional)
              step = 50     # Step size (optional)
            ),
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
    geom_errorbar(aes(y = method, xmin = lower, xmax = upper)) + 
    xlim(input$range[1], input$range[2]) +
    geom_vline(xintercept = 0.95, color = "red", linetype = "dotted") +
    labs(
      x = "Coverage Frequency"
    )
}
