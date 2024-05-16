library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(here)
library(DT)

source("PlotSpecifications.R")
source("plotting.R")
# Sample data frame
data <- readRDS("../results/ci_aggr.rds")

# UI for the main page
ui <- fluidPage(
  titlePanel("Interactive Plot"),
  fluidRow(
    column(12,
      helpText("Explanation of the context goes here..."),
      actionButton("viewPlot", "View Plot"),
      actionButton("viewData", "View Data"),
    br(),
      uiOutput("pageContent")
    ))
  )


# UI for the data page
dataPage <- fluidPage(titlePanel("Data"),
                      mainPanel(width = 12,
                                DT::dataTableOutput("mytable")))

# UI for the plot page
plotPage <- fluidPage(
  titlePanel("Plot View"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
      specifications_ui("fallback"),
      specifications_download("fallback")
    )
    ),
    mainPanel(
      #column(12,
      plotting_ui("fallback")
    )#)
  )
)

# Server logic
server <- function(input, output, session) {
  # Switch between pages
  observeEvent(input$viewData, {
    output$pageContent <- renderUI({
      dataPage
    })
  })
  
  observeEvent(input$viewPlot, {
    output$pageContent <- renderUI({
      plotPage
    })
  })
  
  # Render data table
  output$mytable <- DT::renderDataTable(data,
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
  
  # Render Plotly plot
  callModule(function(input, output, session) {
  output$plot <- renderPlotly({
    fallback_plot(data=aggrs,x = input$x,y = input$y,colorval = input$color,method=input$method)
  })
  
  # Download plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      "plot.png"
    },
    content = function(file) {
      p <- fallback_plot(data=aggrs,x = input$x,y = input$y,colorval = input$color,method=input$method)
      ggsave(file, plot = p, width = as.numeric(input$width), height = as.numeric(input$height), device = "png",units=input$units)
    }
  )  }, "fallback")
}

# Run the application
shinyApp(ui, server)
