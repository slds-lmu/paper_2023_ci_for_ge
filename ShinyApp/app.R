library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(here)
library(DT)
# library(shinyjs)
# library(shinyWidgets)

source("setup.R")
source("explanation.R")
source("make_data.R")
source("plot_specifications.R")
source("Size_x_Coverage.R")
#source("plotting_fallback.R")
# source("0_plot_blueprint.R")   #INSERT FOR NEW PLOTS

ui = fluidPage(
  # Add a custom CSS for the banner
  tags$head(
    tags$style(HTML("
      .header-banner {
        background: url('pattern.svg');
        color: gray;
        padding: 20px;
        text-align: center;
        font-size: 24px;
        font-weight: bold;
        border-radius: 5px;
      }
      .header-banner .title {
        margin: 0;
      }
      .header-banner .subtitle {
        font-size: 16px;
        margin-top: 10px;
      }
      .header-banner .buttons {
        margin-top: 20px;
      }
      .header-banner .buttons .btn {
        margin: 0 10px;
      }
      .footer {
        background-color: #d3d3d3;
        padding: 20px 0;
        text-align: center;
        border-top: 1px solid #a9a9a9;
        position: fixed;
        bottom: 0;
        left: 0;
        width: 100%;
        display: flex;
        flex-direction: column;
        align-items: center;
        z-index: 1000;
      }
      .footer .logos {
        display: flex;
        justify-content: center;
        align-items: center;
        margin-top: 10px;
      }
      .footer .logos img {
        margin: 0 10px;
        height: 40px;
      }
      .content {
        padding-bottom: 100px;  /* Adjust this value if needed */
      }
      .plot-container {
        margin-bottom: 100px;  /* Ensure there's enough space for the footer */
      }
    "))
  ),
  div(
    class = "header-banner",
    h1(class = "title", "CIs for the GE Empirical Results"),
    div(class = "subtitle", "Quick description of what's happening..."),
    div(
      class = "buttons",
      actionButton("viewPlot", "Plots"),
      actionButton("viewData", "View Data"),
      actionButton("viewExplanation", "View explanation again")
    )
  ),
  fluidRow(
    column(
      12,
      br(),
      uiOutput("pageContent")
    )
  ),
  div(
    class = "footer",
    div(
      class = "logos",
      span("A joint project of"),
      img(src = "LMU.png", alt = "Logo 1"),
      img(src = "DESTATIS.png", alt = "Logo 2")
    )
  )
)


# UI for the explanation page
explanationPage = fluidPage(
  titlePanel(""),
  mainPanel(
    width = 12,
    hr(""),
    HTML(html_explanation)
  )
)

# UI for the data page
dataPage = fluidPage(
  titlePanel("Data"),
  mainPanel(
    width = 12,
    DT::dataTableOutput("mytable")
  )
)

# UI for the plot page
plotPage = fluidPage(
  titlePanel("Plot View"),
  fluidPage(
    tabsetPanel(
      #specifications_ui("fallback"),
      #specifications_methodplot("fallback"),
      # specifications_blueprint("fallback"),    #INSERT FOR NEW PLOTS
      tabPanel("Size x Coverage Plots", fluidPage(
        tabsetPanel(
      specifications_learnerplot("SxC_learner"),
      specifications_taskplot("SxC_task"))
      )
    ),
    specifications_download("downloadNS"))
  )
)

server = function(input, output, session) {
  # Switch between pages
  output$pageContent = renderUI({
    explanationPage
  })
  
  observeEvent(input$viewExplanation, {
    output$pageContent = renderUI({
      explanationPage
    })
  })
  
  observeEvent(input$viewData, {
    output$pageContent = renderUI({
      dataPage
    })
  })
  
  observeEvent(input$viewPlot, {
    output$pageContent = renderUI({
      plotPage
    })
  })
  
  
  addon_applied = reactiveVal(NULL)
  observeEvent(input$apply,{
    addon_applied(TRUE)
    }
  )
  
  setNULL(download_vals)
  callModule(function(input, output, session) {
    observe({
    global_units <<- input$units
    global_width <<- input$width
    global_height <<- input$height
    global_code <<- input$code
    })
  },"downloadNS")
  
  
  button_clicked = reactiveVal(NULL)
  
  observeEvent(input$Vlearner, {
    button_clicked("VIEW_learner")
  })
  
  observeEvent(input$Vtask, {
    button_clicked("VIEW_task")
  })
  
  
  
  callModule(function(input, output, session) {
    output$Plearner = renderPlotly({
      clicker = button_clicked()
      g = make_learnerplot(ci_aggr, input)
      makeplot(clicker, "VIEW_learner", g)
    })
    
    output$Dlearner = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_learnerplot(ci_aggr, input)
        print("Cheking...")
        print(addon_applied)
        print(global_units)
        if (!is.null(addon_applied)) {
          code = input$code
          g = g + eval(parse(text = global_code))
        }
        ggsave(file, plot = g, width = as.numeric(global_width), height = as.numeric( global_height), device = "png", units = global_units)
      }
    )
    
  }, "SxC_learner")
  
  
  callModule(function(input, output, session) {
    output$Ptask = renderPlotly({
      clicker = button_clicked()
      g = make_taskplot(ci_aggr, input)
      makeplot(clicker, "VIEW_task", g)
      
    })
    
    output$Dtask = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_taskplot(ci_aggr, input)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = global_code))
        }
        ggsave(file, plot = g, width = as.numeric(global_width), height = as.numeric( global_height), device = "png", units = global_units)
      }
    )
    
  }, "SxC_task")
  
}

# Run the application
shinyApp(ui, server)
