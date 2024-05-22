library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(here)
library(DT)
# library(shinyjs)
library(shinyWidgets)

source("setup.R")
source("datasheet.R")
source("plot_specifications.R")
source("Size_x_Coverage.R")
source("target_comparison.R")
source("width_vs_coverage.R")
source("cis_for_cis.R")

ui <- fluidPage(
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
      actionButton("viewDataSheet", "View Parameter Data"),
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
explanationPage <- fluidPage(
  titlePanel(""),
  mainPanel(
    width = 12,
    hr(""),
    HTML(paste(readLines("explanation.html"),collapse=""))
  )
)

# UI for the data page
dataPage <- fluidPage(
  titlePanel("Data"),
  mainPanel(div(
    class = "content",
    width = 12,
    DT::dataTableOutput("mytable")
  ))
)
dataPage2 <- fluidPage(
  titlePanel("Parameter data"),
  mainPanel(div(
    class = "content",
    width = 12,
    DT::dataTableOutput("data_sheet")
  ))
)

# UI for the plot page
plotPage <- fluidPage(
  titlePanel("Plot View"),
  fluidPage(
    tabsetPanel(
      tabPanel("Size x Coverage Plots", fluidPage(
        tabsetPanel(
          aggregated_Sx_Cplot_ui("SxC_aggr"),
          specifications_methodplot("SxC_method"),
          specifications_learnerplot("SxC_learner"),
          specifications_taskplot("SxC_task")
        )
      )),
      tabPanel("Target Comparison", fluidPage(
        specification_target_comparsion("target_comparison")
      )),
      tabPanel("Width vs. Coverage", fluidPage(
        specification_width_vs_coverage("width_vs_coverage")
      )),
      tabPanel("CIs for CI Coverage", fluidPage(
        specification_cis_for_cis("cis_for_cis")
      )),
      tabPanel("Austern & Zhou", fluidPage()),
      specifications_download("downloadNS")
    )
  )
)

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

  observeEvent(input$viewDataSheet, {
    output$pageContent <- renderUI({
      dataPage2
    })
  })

  observeEvent(input$viewPlot, {
    output$pageContent <- renderUI({
      plotPage
    })
  })


  output$mytable <- DT::renderDataTable(ci_aggr,
    options = list(scrollX = TRUE),
    rownames = FALSE
  )

  output$data_sheet <- DT::renderDataTable(ci_aggr,
    options = list(scrollX = TRUE),
    rownames = FALSE
  )


  addon_applied <- reactiveVal(NULL)
  observeEvent(input$apply, {
    addon_applied(TRUE)
  })

  setNULL(download_vals)
  callModule(function(input, output, session) {
    observe({
      global_units <<- input$units
      global_width <<- input$width
      global_height <<- input$height
      global_code <<- input$code
    })
  }, "downloadNS")


  button_clicked <- reactiveVal(NULL)

  observeEvent(input$Vlearner, {
    button_clicked("VIEW_learner")
  })

  observeEvent(input$Vtarget_comparison, {
    button_clicked("VIEW_target_comparison")
  })

  observeEvent(input$Vwidth_vs_coverage, {
    button_clicked("VIEW_width_vs_coverage")
  })

  observeEvent(input$Vcis_for_cis, {
    button_clicked("VIEW_cis_for_cis")
  })

  observeEvent(input$Vtask, {
    button_clicked("VIEW_task")
  })

  observeEvent(input$Vmethod, {
    button_clicked("VIEW_method")
  })

  observeEvent(input$viewPlot_fallback, {
    button_clicked("VIEW_fallback")
  })




  callModule(function(input, output, session) {
    observe({
      if (is.null(input$method) || length(input$method) == 0) {
        updatePickerInput(session, "method", selected = levels(as.factor(as.data.frame(ci_aggr)$method))[1])
      }
    })

    output$fallbackplot <- renderPlotly({
      clicker <- button_clicked()
      y <- input$y
      g <- fallback_plot(ci_aggr, y = y, input)
      makeplot(clicker, "VIEW_fallback", g)
    })

    output$downloadPlot_fallback <- downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        y <- input$y
        g <- fallback_plot(ci_aggr, y = y, input)
        if (!is.null(addon_applied)) {
          g <- g + eval(parse(text = global_code))
        }
        ggsave(file, plot = g, width = as.numeric(global_width), height = as.numeric(global_height), device = "png", units = global_units)
      }
    )
  }, "SxC_aggr")

  callModule(function(input, output, session) {
    output$Pmethod <- renderPlotly({
      clicker <- button_clicked()
      g <- make_methodplot(ci_aggr, input)
      makeplot(clicker, "VIEW_method", g)
    })

    output$Dmethod <- downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g <- make_methodplot(ci_aggr, input)
        print(addon_applied)
        print(global_units)
        if (!is.null(addon_applied)) {
          g <- g + eval(parse(text = global_code))
        }
        ggsave(file, plot = g, width = as.numeric(global_width), height = as.numeric(global_height), device = "png", units = global_units)
      }
    )
  }, "SxC_method")

  callModule(function(input, output, session) {
    output$Plearner <- renderPlotly({
      clicker <- button_clicked()
      g <- make_learnerplot(ci_aggr, input)
      makeplot(clicker, "VIEW_learner", g)
    })

    output$Dlearner <- downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g <- make_learnerplot(ci_aggr, input)
        print(addon_applied)
        print(global_units)
        if (!is.null(addon_applied)) {
          g <- g + eval(parse(text = global_code))
        }
        ggsave(file, plot = g, width = as.numeric(global_width), height = as.numeric(global_height), device = "png", units = global_units)
      }
    )
  }, "SxC_learner")

  callModule(function(input, output, session) {
    output$Ptarget_comparison <- renderPlotly({
      clicker <- button_clicked()
      g <- make_target_comparison_plot(ci_aggr, input)
      makeplot(clicker, "VIEW_target_comparison", g)
    })

    output$Dtarget_comparison <- downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g <- make_target_comparison_plot(ci_aggr, input)
        print(addon_applied)
        print(global_units)
        if (!is.null(addon_applied)) {
          g <- g + eval(parse(text = global_code))
        }
        ggsave(file, plot = g, width = as.numeric(global_width), height = as.numeric(global_height), device = "png", units = global_units)
      }
    )
  }, "target_comparison")

  callModule(function(input, output, session) {
    output$Pwidth_vs_coverage <- renderPlotly({
      clicker <- button_clicked()
      g <- make_width_vs_coverage_plot(ci_aggr, input)
      makeplot(clicker, "VIEW_width_vs_coverage", g)
    })

    output$Dwidth_vs_coverage <- downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g <- make_width_vs_coverage_plot(ci_aggr, input)
        print(addon_applied)
        print(global_units)
        if (!is.null(addon_applied)) {
          g <- g + eval(parse(text = global_code))
        }
        ggsave(file, plot = g, width = as.numeric(global_width), height = as.numeric(global_height), device = "png", units = global_units)
      }
    )
  }, "width_vs_coverage")

  callModule(function(input, output, session) {
    output$Pcis_for_cis <- renderPlotly({
      clicker <- button_clicked()
      g <- make_cis_for_cis_plot(ci_aggr, input)
      makeplot(clicker, "VIEW_cis_for_cis", g)
    })

    output$Dcis_for_cis <- downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g <- make_cis_for_cis_plot(ci_aggr, input)
        print(addon_applied)
        print(global_units)
        if (!is.null(addon_applied)) {
          g <- g + eval(parse(text = global_code))
        }
        ggsave(file, plot = g, width = as.numeric(global_width), height = as.numeric(global_height), device = "png", units = global_units)
      }
    )
  }, "cis_for_cis")


  callModule(function(input, output, session) {
    output$Ptask <- renderPlotly({
      clicker <- button_clicked()
      g <- make_taskplot(ci_aggr, input)
      makeplot(clicker, "VIEW_task", g)
    })

    output$Dtask <- downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g <- make_taskplot(ci_aggr, input)
        if (!is.null(addon_applied)) {
          g <- g + eval(parse(text = global_code))
        }
        ggsave(file, plot = g, width = as.numeric(global_width), height = as.numeric(global_height), device = "png", units = global_units)
      }
    )
  }, "SxC_task")
}

# Run the application
shinyApp(ui, server)
