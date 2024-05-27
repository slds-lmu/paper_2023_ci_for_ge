specification_chen_10_null = function(id) {
  ns = NS(id)
  tabPanel(
    "Chen 10 Null",
    div(
      class = "content",
      fluidRow(column(12,
        HTML(paste(readLines("HTMLS/chen_10_null.html"), collapse = "")))),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("target"), "Target:", choices = c("Risk", "Expected Risk", "Proxy Quantity"), "Risk"),
              pickerInput(ns("methods"), "Methods:",
                choices = METHODS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = METHODS
              ),
              pickerInput(ns("sizes"), "Sizes:",
                choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)),
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = as.character(c(100L, 500L, 1000L, 5000L, 10000L))
              ),
              pickerInput(ns("inducers"), "Inducers:",
                choices = INDUCERS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = INDUCERS),
              br(),
              actionButton("Vchen_10_null", "View Plot"),
              downloadButton(ns("Dchen_10_null"), "Download Plot")
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
            plotlyOutput(ns("Pchen_10_null"))
          )
        )
    ))
  )
}


make_chen_10_null_plot = function(data, input, globalOps) {
  target = translate_target(input$target)

  data = data[
    size %in% as.integer(input$sizes) &
      measure %in% translate_losses(input$loss_regr, input$loss_classif) &
      method %in% input$methods &
      inducer %in% input$inducers,
      list(
        cov = mean(get(paste0("cov_", target)))
    ),
    by = c("inducer", "size", "method")
  ]

  data = data[!is.na(cov)]
  data$method = droplevels(data$method)

  ggplot(data, aes(y = method, x = cov)) +
    facet_grid(vars(inducer), vars(size)) +
    geom_point() + 
    geom_vline(xintercept = 0.95, color = "red") +
    xlim(NA, 1) + 
    labs(
      x = "Coverage Frequency",
      y = "Inference Method"
    )
  }
