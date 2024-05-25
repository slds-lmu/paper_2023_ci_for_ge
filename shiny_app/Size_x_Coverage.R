specifications_aggregated = function(id) {
  ns = NS(id)
  tabPanel(
    "Method, aggregated",
    div(
      class = "content",
      fluidRow(column(12,
        HTML(paste(readLines("HTMLS/SxC_method_aggregated.html"), collapse = "")))),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
              # helpText("Select variables for plotting:"),
              sliderInput(ns("slider1"), "y Min", min = 0, max = 1, value = 0),
              selectInput(ns("free_scales"), "Free Scales:", choices = c("x", "y", "both", "none"), "none"),
              sliderInput(ns("slider2"), "y Max", min = 0, max = 1, value = 1),
              selectInput(ns("min_size"), "Min Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
              selectInput(ns("max_size"), "Max Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "10000"),
              selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
              selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
              selectInput(ns("y"), "Target Quantity:", choices = c("Risk", "Expected Risk", "Proxy Quantity", "all")),
              selectInput(ns("evaluation"), "Evaluation:", choices = c("Coverage Error", "Average Coverage"), "Coverage Error"),
              selectInput(ns("sep_group"), "Separately show:", choices = c("dgp", "inducer", "none")),
              pickerInput(ns("method"), "Method:",
                choices = METHODS,
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                METHODS[1]
              ),
              br(),
              actionButton("viewPlot_fallback", "View Plot"),
              downloadButton(ns("downloadPlot_fallback"), "Download Plot")
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
            plotlyOutput(ns("fallbackplot"))
          )
          )
        )
    ))
}


fallback_plot = function(data, input, globalOps) {
  data = data[as.character(method) %in% globalOps$methods_global() & as.character(dgp) %in% globalOps$dgps_global() ]
  y = switch(input$y,
    "Risk" = "y_R",
    "Expected Risk" = "y_ER",
    "Proxy Quantity" = "y_PQ",
    "all" = "all"
  )

  line = NULL
  colorval = NULL
  if (input$sep_group != "none") {
    colorval = input$sep_group
    aggregate = setdiff(c("dgp", "inducer"), colorval)
  } else {
    aggregate = c("dgp", "inducer")
  }

  min_size = as.integer(input$min_size)
  max_size = as.integer(input$max_size)

  scales = translate_scales(input$free_scales)

  min_y = input$slider1
  max_y = input$slider2

  vec = c("inducer", "dgp", "method", "size")
  if (!is.null(aggregate)) {
    vec = setdiff(vec, aggregate)
  }

  newdat = if (input$evaluation == "Coverage Error") {
   data[method %in% input$method &
    size >= min_size & size <= max_size & measure %in% translate_losses(input$loss_regr, input$loss_classif),
    list(
      y_R = sqrt(mean((cov_R - 0.95)^2)),
      y_ER = sqrt(mean((cov_ER - 0.95)^2)),
      y_PQ = sqrt(mean((cov_PQ - 0.95)^2))
      ), by = vec]
  } else {
   data[method %in% input$method &
    size >= min_size & size <= max_size & measure %in% translate_losses(input$loss_regr, input$loss_classif),
    list(
      y_R = mean(cov_R),
      y_ER = mean(cov_ER),
      y_PQ = mean(cov_PQ)
      ), by = vec]
  }


  if (y == "all") {
    line = TRUE
    newdat = melt(newdat, setdiff(names(newdat), c("y_R", "y_ER", "y_PQ")),
      variable.name = "line",
      value.name = "y"
    )

    newdat$line = as.factor(ifelse(newdat$line == "y_R", "Risk", ifelse(newdat$line == "y_ER", "Expected Risk", "Proxy Quantity")))
  }

  if (min_size == max_size) {
    output = ggplot(newdat, aes_string(x = if (!is.null(line)) "y" else y, y = colorval, color = if (!is.null(line)) "line")) + 
      geom_point() +
      facet_wrap(vars(method), scales = scales) + 
      labs(
        x = input$evaluation,
        y = input$sep_group
      )

    if (input$evaluation == "Average Coverage") {
      output = output + geom_vline(xintercept = 0.95, color = "red")
    }
  } else {
    add = list(facet_wrap(vars(method), scales = scales),
      geom_line(),
      xlim(min_size, max_size),
      ylim(min_y, max_y))

    if (!is.null(colorval) & is.null(line)) {
      output = ggplot(newdat, aes_string(x = "size", y = y, color = colorval)) + add
    }
    if (is.null(colorval) & is.null(line)) {
      output = ggplot(newdat, aes_string(x = "size", y = y)) + add
    }
    if (is.null(colorval) & !is.null(line)) {
      output = ggplot(newdat, aes_string(x = "size", y = "y", linetype = "line")) + add
    }
    if (!is.null(colorval) & !is.null(line)) {
      output = ggplot(newdat, aes_string(x = "size", y = "y", color = colorval, linetype = "line")) + add
    }

    output = if (input$evaluation == "Coverage Error") {
      output + labs(
        y = "Coverage Error (rmse)",
        x = "Dataset Size"
      )
    } else {
      output + labs(
        y = "Average Coverage",
        x = "Dataset Size"
      ) + 
      geom_hline(yintercept = 0.95, color = "red")
    }
  }

  return(output)
}

# creates a plot specification
specification_factory = function(atom, view_name, download_name, plot_name) {
  stopifnot(atom %in% c("inducer", "method", "dgp"))
  choices = c("inducer", "method", "dgp", "none")
  choices = choices[choices != atom]

  atom_choices = ATOM_CHOICES[[atom]]

  f = function(id) {
    ns = NS(id)
    tabPanel(
      paste("Single", if (atom == "dgp") "DGP" else capitalize(atom)),
      div(
        class = "content",
      fluidRow(column(12,
        HTML(paste(readLines(sprintf("HTMLS/SxC_%s.html", atom)), collapse = "")))),
        sidebarLayout(
          sidebarPanel(

            fluidRow(
              column(
                6,
                # helpText("Select variables for plotting:"),
                sliderInput(ns("slider1"), "y Min", min = 0, max = 1, value = 0),
                sliderInput(ns("slider2"), "y Max", min = 0, max = 1, value = 1),
                selectInput(ns("free_scales"), "Free Scales:", choices = c("x", "y", "both", "none"), "none"),
                selectInput(ns("min_size"), "Min Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
                selectInput(ns("max_size"), "Max Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "10000"),
                selectInput(ns("evaluation"), "Evaluation:", choices = c("Coverage Error", "Average Coverage"), "Coverage Error"),
                selectInput(ns("loss_regr"), "Loss (regr):", LOSSES$regr, "Squared"),
                selectInput(ns("loss_classif"), "Loss (classif):", LOSSES$classif, "Zero-One"),
                selectInput(ns("target"), "Target Quantity:", choices = c("Risk", "Expected Risk", "Proxy Quantity"), selected = "Risk"),
                selectInput(ns("group"), "Group:", choices = choices, choices[1L]),
                selectInput(ns("color"), "Color:", choices = choices, choices[2L]),
                selectInput(ns("atom"), paste0(capitalize(atom), ":"), choices = atom_choices),
                br(),
                actionButton(view_name, "View Plot"),
                downloadButton(ns(download_name), "Download Plot")
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
              plotlyOutput(ns(plot_name)) ## we can define height + width as needed!
              # also as a function of inputs!
            )
          )
        )
      )
    )
  }
  return(f)
}

plotter_factory = function(atom) {
  function(data, input, globalOps) {
    data = data[as.character(method) %in% globalOps$methods_global() & as.character(dgp) %in% globalOps$dgps_global() ]
    min_size = as.integer(input$min_size)
    max_size = as.integer(input$max_size)
    target = translate_target(input$target)
    by_vars = c("size", if (input$group != "none") input$group, if (input$color != "none") input$color)

    scales = translate_scales(input$free_scales)
    
    min_y = input$slider1
    max_y = input$slider2

    data = if (input$evaluation == "Coverage Error") {
      data[
        data[[atom]] == input$atom & size >= min_size & size <= max_size & measure %in% translate_losses(input$loss_regr, input$loss_classif),
        list(
          y = sqrt(mean((get(paste0("cov_", target)) - 0.95)^2))
        ),
        by = by_vars
      ] 
    } else {
      data[
        data[[atom]] == input$atom & size >= min_size & size <= max_size & measure %in% translate_losses(input$loss_regr, input$loss_classif),
        list(
          y = mean(get(paste0("cov_", target)))
        ),
        by = by_vars
      ]
    }

    if (min_size == max_size) {
      p = ggplot(data, aes_string(x = "y", y = input$color)) + geom_point()
      if (input$group != "none") p = p + facet_wrap(as.formula(paste0("~", input$group)), scales = scales)
      p + labs(
          x = paste0(input$evaluation, " for ", input$target),
          y = input$color
        ) + xlim(min_y, max_y)

      if (input$evaluation == "Average Coverage") {
        p = p + geom_vline(xintercept = 0.95, color = "red")
      }
      p
    } else {
      p = ggplot(data, aes_string(x = "size", y = "y", color = if (input$color != "none") input$color))
      if (input$group != "none")  p = p + facet_wrap(as.formula(paste0("~", input$group)), scales = scales)
      p + labs(
        x = "Dataset Size",
        y = paste0(input$evaluation, " for ", input$target) 
      ) +
       ylim(min_y, max_y) +
       geom_line()
    }
  }
}

# primary view: inducer
specifications_inducerplot = specification_factory("inducer", "Vinducer", "Dinducer", "Pinducer")
make_inducerplot = plotter_factory("inducer")

# primary view: dgp
specifications_dgpplot = specification_factory("dgp", "Vdgp", "Ddgp", "Pdgp")
make_dgpplot = plotter_factory("dgp")

# primary view: method
specifications_methodplot = specification_factory("method", "Vmethod", "Dmethod", "Pmethod")
make_methodplot = plotter_factory("method")
