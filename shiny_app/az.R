
specification_az = function(id) {
  ns = NS(id)
  tabPanel(
    "Austern & Zhou",
    div(
      class = "content",
      fluidRow(column(12,
        HTML(paste(readLines("HTMLS/az.html"), collapse = "")))),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              br(),
              actionButton("Vaz", "View Plot"),
              downloadButton(ns("Daz"), "Download Plot")
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
            plotlyOutput(ns("Paz"))
          )
        )
    ))
  )
}


make_az_plot = function(data, input, globalOps) {
  az = data
  az$ratio = az$est / az$truth
  y_breaks = c(0, 0.5, 1, sqrt(2), 2)
  y_labels = c("0", "0.5", "1", "sqrt(2)", "2") 
  
  x_breaks = c(1000, 5000, 10000)
  x_labels = as.character(x_breaks)
  
  p = ggplot(az, aes(x = n, y = ratio)) + 
  geom_point() + 
  labs(
    x = "n",
    y = "Ratio of SD (ROCV) to true SD of 5-fold CV") +
    geom_hline(yintercept = sqrt(2), linetype = "dotted", color = "red") + 
    scale_y_continuous(breaks = y_breaks, labels = y_labels, limits = c(0, 2)) + 
    scale_x_continuous(breaks = x_breaks, labels = x_labels)
  }
