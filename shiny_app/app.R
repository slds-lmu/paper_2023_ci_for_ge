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
source("plot_specifications.R")
source("plotting_fallback.R")
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
      specifications_ui("fallback"),
      specifications_methodplot("fallback"),
      # specifications_blueprint("fallback"),    #INSERT FOR NEW PLOTS
      specifications_download("fallback")
    )
  )
)

# Server logic
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

  # Render data table
  output$mytable = DT::renderDataTable(data,
    options = list(scrollX = TRUE),
    rownames = FALSE
  )
  # Plotions
  button_clicked = reactiveVal(NULL)
  observeEvent(input$viewPlot_fallback, {
    button_clicked("FALLBACK")
  })
  observeEvent(input$viewPlot_method, {
    button_clicked("METHOD")
  })
  # observeEvent(input$viewPlot_blueprint, {   #INSERT FOR NEW PLOTS
  #  button_clicked("BLUEPRINT")
  # })

  # Render Plotly plot

  callModule(function(input, output, session) {
    output$fallbackplot = renderPlotly({
      clicker = button_clicked()
      g = fallback_plot(data = aggrs, x = input$x, y = input$y, colorval = input$color_fallback, method = input$method)
      makeplot(clicker, "FALLBACK", g)
    })
    output$methodplot = renderPlotly({
      clicker = button_clicked()
      meth = input$methodOI
      method_dat = as.data.frame(aggrs_base[which(aggrs_base$method == meth), ])
      g = method_plot(data = method_dat, x = input$x1, y = input$y1, colorval = input$color_method)
      makeplot(clicker, "METHOD", g)
    })
    # output$blueprintplot = renderPlotly({    #INSERT FOR NEW PLOTS
    #  clicker = button_clicked()
    #  g = blueprint_plot(specify inputs from specifications_blueprint...)
    #  makeplot(clicker,"BLUEPRINT",g)
    # })
  }, "fallback")

  callModule(function(input, output, session) {
    addon_applied = reactiveVal(NULL)
    observeEvent(
      input$apply,
      addon_applied(TRUE)
    )
    # Download plots
    output$downloadPlot_fallback = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = fallback_plot(data = aggrs, x = input$x, y = input$y, colorval = input$color_fallback, method = input$method)
        if (!is.null(addon_applied)) {
          code = input$code
          g = g + eval(parse(text = code))
        }
        print(input$units)
        ggsave(file, plot = g, width = as.numeric(input$width), height = as.numeric(input$height), device = "png", units = input$units)
      }
    )
    output$downloadPlot_method = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        method_dat = as.data.frame(aggrs_base[which(aggrs_base$method == input$methodOI)])
        g = method_plot(data = method_dat, x = input$x1, y = input$y1, colorval = input$color_method)
        if (!is.null(addon_applied)) {
          code = input$code
          g = g + eval(parse(text = code))
        }
        ggsave(file, plot = g, width = as.numeric(input$width), height = as.numeric(input$height), device = "png", units = input$units)
      }
    )

    # output$downloadPlot_blueprint = downloadHandler(
    #  filename = function() {
    #    "plot.png"
    #  },
    #  content = function(file) {
    #    g = blueprint_plot(specify inputs from specifications_blueprint...)
    #   if(!is.null(addon_applied)){
    #   code = input$code
    #     g = g + eval(parse(text=code))
    #   }
    #     ggsave(file, plot = g, width = as.numeric(input$width), height = as.numeric(input$height), device = "png",units=input$units)
    #  }
    # )
  }, "fallback")
}

# Run the application
shinyApp(ui, server)
