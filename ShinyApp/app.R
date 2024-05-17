library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(here)
library(DT)

source("PlotSpecifications.R")
source("plotting_fallback.R")
# Sample data frame
data <- readRDS("../results/ci_aggr.rds")

# UI for the main page
ui <- fluidPage(
  titlePanel("CIs for the GE empirical results"),
  fluidRow(
    column(12,
      helpText("Quick description of what's happening...."),
      actionButton("viewPlot", "Plots"),
      actionButton("viewData", "View Data"),
      actionButton("viewExplanation", "View explanation again"),
    br(),
      uiOutput("pageContent")
    ))
  )

# UI for the explanation page
explanationPage <- fluidPage(titlePanel(""),
                      mainPanel(width = 12,
                                hr(""),
                                HTML("<div style='text-align:center;'><p>
                                     Method x Learner x DGP x size
                                     </p><p>&nbsp;</p><hr><p>&nbsp;</p></div>")
                                ))

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
      specifications_methodplot("fallback"),
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
  output$pageContent <- renderUI({
    explanationPage
  })
  
  observeEvent(input$viewExplanation, {
    output$pageContent <- renderUI({
      explanationPage
    })
  })
  
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
#Plotions
  button_clicked <- reactiveVal(NULL)
  observeEvent(input$viewPlot_fallback, {
    button_clicked("FALLBACK")
  })
  observeEvent(input$viewPlot_method, {
    button_clicked("METHOD")
  })
  
  # Render Plotly plot
  callModule(function(input, output, session) {
  output$plot <- renderPlotly({
    clicker <- button_clicked()
    if(is.null(clicker)){ggplot()+theme_minimal()}else{
    if(clicker=="FALLBACK"){
    fallback_plot(data=aggrs,x = input$x,y = input$y,colorval = input$color_fallback,method=input$method)
    }else{
      if(clicker=="METHOD"){
        method_dat <- as.data.frame(aggrs_base[which(aggrs_base$method==input$methodOI)])
        method_plot(data=method_dat,x = input$x1,y = input$y1,colorval = input$color_method)  
      }}  
    }
  })
  
  # Download plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      "plot.png"
    },
    content = function(file) {
      clicker <- button_clicked()
      if(is.null(clicker)){ggplot()+theme_minimal()}else{
        if(clicker=="FALLBACK"){
      p <- fallback_plot(data=aggrs,x = input$x,y = input$y,colorval = input$color,method=input$method)
        }else{
          if(clicker=="METHOD"){
            method_dat <- as.data.frame(aggrs_base[which(aggrs_base$method==input$methodOI)])
       p <- method_plot(data=method_dat,x = input$x1,y = input$y1,colorval = input$color_method)  
          }}  
        }
      ggsave(file, plot = p, width = as.numeric(input$width), height = as.numeric(input$height), device = "png",units=input$units)
    }
  )  }, "fallback")
}

# Run the application
shinyApp(ui, server)
