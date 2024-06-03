source("setup.R")
source("datasheet.R")
source("plot_specifications.R")
source("Size_x_Coverage.R")
source("target_comparison.R")
source("width_vs_coverage.R")
source("chen_10_null.R")
source("inducer_performance.R")
source("under_vs_over.R")
source("cis_for_cis.R")
source("global_options.R")
source("az.R")


ui = fluidPage(
  # Add a custom CSS for the banner
  tags$head(
    tags$style(HTML(paste(readLines("HTMLS/style.html"), collapse = "")))
  ),
  div(
    class = "header-banner",
    h1(class = "title", "CIs for the GE: Empirical Results of the Benchmark Study"),
    div(class = "subtitle", 
        "A supplement to", tags$em("“Constructing confidence intervals for “the” Generalization
Error”")),
    div(
      class = "buttons",
      actionButton("viewExplanation", "Overview"),
      actionButton("viewData", "Data"),
      actionButton("viewPlot", "Plots")
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
  withMathJax(),
  titlePanel(""),
  mainPanel(
    width = 12,
    hr(""),
    HTML(paste(readLines("HTMLS/explanation.html"), collapse = ""))
  )
)

# UI for the data page
dataPage = fluidPage(
  withMathJax(),
  titlePanel("Data"),
  fluidRow(hr(""),
           HTML(paste(readLines("HTMLS/data_explanation.html"), collapse = ""))),
  tabsetPanel(
    tabPanel("DGPs",
             mainPanel(div(
               class = "content",
               width = 12,
               DT::dataTableOutput("data_overview")
             ))),
    tabPanel("Inference Methods",
             mainPanel(div(
               class = "content",
               width = 12,
               DT::dataTableOutput("method_overview")
             ))),
    tabPanel("Inducers",
             mainPanel(div(
               class = "content",
               width = 12,
               DT::dataTableOutput("inducer_overview")
             ))),
    tabPanel("Loss Functions",
             mainPanel(div(
               class = "content",
               width = 12,
               DT::dataTableOutput("loss_overview")
             ))),
    tabPanel("Results",
             mainPanel(div(
               class = "content",
               width = 12,
               DT::dataTableOutput("result_overview")
             )))
  )
)

# UI for the plot page
plotPage = fluidPage(
  titlePanel("Plot View"),
  fluidPage(
    withMathJax(),
    tabsetPanel(
      specifications_DataOps("data_opsNS"),
      tabPanel("Size vs. Coverage", fluidPage(
        withMathJax(),
        tabsetPanel(
          specifications_aggregated("SxC_aggr"),
          specifications_methodplot("SxC_method"),
          specifications_inducerplot("SxC_inducer"),
          specifications_dgpplot("SxC_dgp")
        )
      )),
      tabPanel("Target Comparison", fluidPage(
        withMathJax(),
        specification_target_comparsion("target_comparison")
      )),
      tabPanel("Width vs. Cov.", fluidPage(
        withMathJax(),
        specification_width_vs_coverage("width_vs_coverage")
      )),
      tabPanel("Under/Over-Cov", fluidPage(
        withMathJax(),
        specification_under_vs_over("under_vs_over")
      )),
      tabPanel("CIs for Coverage", fluidPage(
        withMathJax(),
        specification_cis_for_cis("cis_for_cis")
      )),
      tabPanel("Chen 10 Null", fluidPage(
        withMathJax(),
        specification_chen_10_null("chen_10_null")
      )),
      tabPanel("Inducer Performance", fluidPage(
        withMathJax(),
        specification_inducer_performance("inducer_performance")
      )),
      tabPanel("Austern & Zhou", fluidPage(
        withMathJax(),
        specification_az("az")
      )),
      specifications_download("downloadNS")
    )
  )
)

server = function(input, output, session) {
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
  
  observeEvent(input$viewDataSheet, {
    output$pageContent = renderUI({
      dataPage2
    })
  })
  
  observeEvent(input$viewDataOverview, {
    output$pageContent = renderUI({
      
      dataDataOverview
    })
  })
  
  observeEvent(input$viewPlot, {
    output$pageContent = renderUI({
      plotPage
    })
  })
  
  
  output$result_overview = DT::renderDataTable(ci_aggr_overview,
                                               options = list(scrollX = TRUE),
                                               rownames = FALSE
  )
  output$loss_overview = DT::renderDataTable(LOSS_OVERVIEW,
                                             options = list(scrollX = TRUE),
                                             rownames = FALSE
  )
  output$inducer_overview = DT::renderDataTable(INDUCER_OVERVIEW,
                                                options = list(scrollX = TRUE),
                                                rownames = FALSE
  )
  
  output$data_sheet = DT::renderDataTable(ci_aggr,
                                          options = list(scrollX = TRUE),
                                          rownames = FALSE
  )
  
  output$data_overview = DT::renderDataTable(DATA_OVERVIEW,
                                             options = list(scrollX = TRUE),
                                             rownames = FALSE
  )
  
  output$method_overview = DT::renderDataTable(METHOD_OVERVIEW,
                                               options = list(scrollX = TRUE),
                                               rownames = FALSE
  )
  
  
  addon_applied = reactiveVal(NULL)
  observeEvent(input$apply, {
    addon_applied(TRUE)
  })
  
  download_global <- inputModuleServer("downloadNS",vals=list("units","width","height","code"))
  globalOps <- inputModuleServer("data_opsNS",vals=list("dgps_global", "methods_global")) #Add new global choices!!!
  
  
  button_clicked = reactiveVal(NULL)
  
  observeEvent(input$Vinducer, {
    button_clicked("VIEW_inducer")
  })
  
  observeEvent(input$Vaz, {
    button_clicked("VIEW_az")
  })
  
  observeEvent(input$Vinducer_performance, {
    button_clicked("VIEW_inducer_performance")
  })
  
  
  observeEvent(input$Vtarget_comparison, {
    button_clicked("VIEW_target_comparison")
  })
  
  observeEvent(input$Vwidth_vs_coverage, {
    button_clicked("VIEW_width_vs_coverage")
  })
  
  observeEvent(input$Vunder_vs_over, {
    button_clicked("VIEW_under_vs_over")
  })
  
  observeEvent(input$Vcis_for_cis, {
    button_clicked("VIEW_cis_for_cis")
  })
  
  observeEvent(input$Vchen_10_null, {
    button_clicked("VIEW_chen_10_null")
  })
  
  observeEvent(input$Vdgp, {
    button_clicked("VIEW_dgp")
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
    observe({
      methods = globalOps$methods_global()
      updatePickerInput(session, "method", choices = methods, selected = methods)
    })
    
    output$fallbackplot = renderPlotly({
      clicker = button_clicked()
      y = input$y
      g = fallback_plot(ci_aggr, input, globalOps)
      makeplot(clicker, "VIEW_fallback", g)
    })
    observe({
      plotlyProxy("fallbackplot", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    
    
    output$downloadPlot_fallback = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        y = input$y
        g = fallback_plot(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "SxC_aggr")
  
  callModule(function(input, output, session) {
    observe({
      methods = globalOps$methods_global()
      updateSelectInput(session, "atom",choices = methods, selected = if (length(methods)) methods[1L])
    })
    output$Pmethod = renderPlotly({
      clicker = button_clicked()
      g = make_methodplot(ci_aggr, input, globalOps)
      makeplot(clicker, "VIEW_method", g)
    })
    observe({
      plotlyProxy("Pmethod", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    
    output$Dmethod = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_methodplot(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "SxC_method")
  
  callModule(function(input, output, session) {
    
    output$Pinducer = renderPlotly({
      clicker = button_clicked()
      g = make_inducerplot(ci_aggr, input, globalOps)
      makeplot(clicker, "VIEW_inducer", g)
    })
    observe({
      plotlyProxy("Pinducer", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    
    output$Dinducer = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_inducerplot(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "SxC_inducer")
  
  callModule(function(input, output, session) {
    observe({
      dgps = globalOps$dgps_global()
      updatePickerInput(session, "dgps", choices = dgps, selected = dgps)
    })
    
    observe({
      plotlyProxy("Ptarget_comparison", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    
    observe({
      methods = PQ_METHODS[PQ_METHODS %in% globalOps$methods_global()]
      updateSelectInput(session, "method",choices = methods, selected = if (length(methods)) methods[1L])
    })
    
    output$Ptarget_comparison = renderPlotly({
      clicker = button_clicked()
      g = make_target_comparison_plot(ci_aggr, input, globalOps)
      makeplot(clicker, "VIEW_target_comparison", g)
    })
    
    output$Dtarget_comparison = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_target_comparison_plot(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "target_comparison")
  
  callModule(function(input, output, session) {
    observe({
      dgps = globalOps$dgps_global()
      updatePickerInput(session, "dgps", choices = dgps, selected = dgps)
    })
    observe({
      methods = globalOps$methods_global()
      updatePickerInput(session, "methods", choices = methods, selected = methods)
    })
    
    observe({
      plotlyProxy("Pwidth_vs_coverage", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    output$Pwidth_vs_coverage = renderPlotly({
      clicker = button_clicked()
      g = make_width_vs_coverage_plot(ci_aggr, input, globalOps)
      makeplot(clicker, "VIEW_width_vs_coverage", g)
    })
    
    output$Dwidth_vs_coverage = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_width_vs_coverage_plot(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "width_vs_coverage")
  
  callModule(function(input, output, session) {
    observe({
      dgps = globalOps$dgps_global()
      updatePickerInput(session, "dgps", choices = dgps, selected = dgps)
    })
    observe({
      methods = globalOps$methods_global()
      updatePickerInput(session, "methods", choices = methods, selected = methods)
    })
    output$Punder_vs_over = renderPlotly({
      clicker = button_clicked()
      g = make_under_vs_over_plot(ci_aggr, input, globalOps)
      makeplot(clicker, "VIEW_under_vs_over", g)
    })
    
    observe({
      plotlyProxy("Punder_vs_over", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    
    output$Dunder_vs_over = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_under_vs_over_plot(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "under_vs_over")
  
  
  callModule(function(input, output, session) {
    observe({
      dgps = globalOps$dgps_global()
      updatePickerInput(session, "dgp", choices = dgps, selected = if (length(dgps)) dgps[1])
    })
    observe({
      methods = globalOps$methods_global()
      updatePickerInput(session, "methods", choices = methods, selected = methods)
    })
    observe({
      plotlyProxy("Pcis_for_cis", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    output$Pcis_for_cis = renderPlotly({
      clicker = button_clicked()
      g = make_cis_for_cis_plot(ci_aggr, input, globalOps)
      makeplot(clicker, "VIEW_cis_for_cis", g)
    })
    
    
    
    output$Dcis_for_cis = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_cis_for_cis_plot(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "cis_for_cis")
  
  callModule(function(input, output, session) {
    output$Pinducer_performance = renderPlotly({
      clicker = button_clicked()
      g = make_inducer_performance_plot(ci_aggr, input, globalOps)
      makeplot(clicker, "VIEW_inducer_performance", g)
    })

    
    observe({
      plotlyProxy("Pinducer_performance", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    
    output$Dinducer_performance = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_inducer_performance_plot(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "inducer_performance")
  
  callModule(function(input, output, session) {
    observe({
      dgps = globalOps$dgps_global()
      updatePickerInput(session, "dgps", choices = dgps, selected = dgps)
    })
    observe({
      methods = globalOps$methods_global()
      updatePickerInput(session, "methods", choices = methods, selected = methods)
    })
    
    observe({
      plotlyProxy("Pchen_10_null", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    
    output$Pchen_10_null = renderPlotly({
      clicker = button_clicked()
      g = make_chen_10_null_plot(ci_aggr_null, input, globalOps)
      makeplot(clicker, "VIEW_chen_10_null", g)
    })
    
    output$Dchen_10_null = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_chen_10_null_plot(ci_aggr_null, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "chen_10_null")
  
  callModule(function(input, output, session) {
    observe({
      dgps = globalOps$dgps_global()
      updateSelectInput(session, "atom", choices = dgps, selected = if (length(dgps)) dgps[1])
    })
    
    output$Pdgp = renderPlotly({
      clicker = button_clicked()
      g = make_dgpplot(ci_aggr, input, globalOps)
      makeplot(clicker, "VIEW_dgp", g)
    })
    
    observe({
      plotlyProxy("Pdgp", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    
    output$Ddgp = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_dgpplot(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "SxC_dgp")
  
  callModule(function(input, output, session) {
    output$Paz = renderPlotly({
      clicker = button_clicked()
      g = make_az_plot(az, input, globalOps)
      makeplot(clicker, "VIEW_az", g)
    })
    
    observe({
      plotlyProxy("Paz", session) %>%
        plotlyProxyInvoke("relayout", list(height = input$height_input))
    })
    
    output$Ddgp = downloadHandler(
      filename = function() {
        "plot.png"
      },
      content = function(file) {
        g = make_daz(ci_aggr, input, globalOps)
        if (!is.null(addon_applied)) {
          g = g + eval(parse(text = download_global$code()))
        }
        ggsave(file, plot = g, width = as.numeric(download_global$width()), height = as.numeric(download_global$height()), device = "png", units = download_global$units())
      }
    )
  }, "az")
}
