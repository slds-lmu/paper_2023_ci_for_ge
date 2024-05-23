# agreggated

aggregated_Sx_Cplot_ui = function(id) {
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
              sliderInput(ns("slider2"), "y Max", min = 0, max = 1, value = 1),
              selectInput(ns("min_size"), "Min Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
              selectInput(ns("max_size"), "Max Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "10000"),
              selectInput(ns("y"), "Y-Axis Variable:", choices = c("avg_cov_R", "avg_cov_ER", "avg_cov_PQ", "all")),
              selectInput(ns("sep_group"), "Separately show:", choices = c("task", "learner", "none")),
              pickerInput(ns("method"), "Method:",
                choices = levels(as.factor(as.data.frame(ci_aggr)$method)),
                multiple = TRUE,
                options = list(`actions-box` = TRUE),
                selected = levels(as.factor(as.data.frame(ci_aggr)$method))[1]),
              br(),
              actionButton("viewPlot_fallback", "View Plot"),
              downloadButton(ns("downloadPlot_fallback"), "Download Plot")
            )
          )
        ),
        mainPanel(
          div(
            class = "plot-container",
            # uiOutput(ns("dynamicPlotContainer"))
            plotlyOutput(ns("fallbackplot"))
          )
        )
    ))
  )
}



fallback_plot = function(data, y, input) {
  line = NULL
  colorval = NULL
  if (input$sep_group != "none") {
    colorval = input$sep_group
    aggregate = setdiff(c("task", "learner"), colorval)
  } else {
    aggregate = c("task", "learner")
  }

  min_size = as.integer(input$min_size)
  max_size = as.integer(input$max_size)

  min_y = as.integer(input$slider1)
  max_y = as.integer(input$slider2)

  vec = c("learner", "task", "method", "size")
  if (!is.null(aggregate)) {
    vec = setdiff(vec, aggregate)
  }


  newdat = data[method %in% input$method &
    size >= min_size & size <= max_size,
  list(avg_cov_R = mean(cov_R),
    avg_cov_ER = mean(cov_ER),
    avg_cov_PQ = mean(cov_PQ)
  ), by = vec]

  if (y == "all") {
    line = TRUE
    newdat = melt(newdat, setdiff(names(newdat), c("avg_cov_R", "avg_cov_ER", "avg_cov_PQ")),
      variable.name = "line",
      value.name = "y"
    )
  }

  add = list(facet_wrap(vars(method), scales = "free_x"),
    geom_hline(yintercept = 0.95, color = "red"),
    geom_line(),
    xlim(min_size, max_size),
    ylim(min_y, max_y))

  if (!is.null(colorval) & is.null(line)) {
    output = ggplot(newdat, aes_string(x = "size", y = y, color = colorval)) + add
  }
  if (is.null(colorval) & is.null(line)) {
    output = ggplot(newdat, aes_string(x = "size", y = y)) + add
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
  return(output)
}


# creates a plot specification
specification_factory = function(atom, view_name, download_name, plot_name) {
  stopifnot(atom %in% c("learner", "method", "task"))
  choices = c("learner", "method", "task", "none")
  choices = choices[choices != atom]


  atom_choices = ATOM_CHOICES[[atom]]

  f = function(id) {
    ns = NS(id)
    tabPanel(
      capitalize(atom),
      div(
        class = "content",
        sidebarLayout(
          sidebarPanel(
            fluidRow(
              column(
                6,
                # helpText("Select variables for plotting:"),
                selectInput(ns("min_size"), "Min Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
                selectInput(ns("max_size"), "Max Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "10000"),
                selectInput(ns("target"), "Target Quantity:", choices = c("Risk", "Expected Risk", "Proxy Quantity"), selected = "Risk"),
                selectInput(ns("group"), "Group By:", choices = choices, choices[1L]),
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
  function(data, input) {
    min_size = as.integer(input$min_size)
    max_size = as.integer(input$max_size)
    target = translate_target(input$target)
    by_vars = c("size", if (input$group != "none") input$group, if (input$color != "none") input$color)


    data = data[
      data[[atom]] == input$atom & size >= min_size & size <= max_size,
      list(cov = mean(get(paste0("cov_", target)))),
      by = by_vars
    ]

    p = if (input$color == "none") {
      ggplot(data, aes_string(x = "size", y = "cov"))
    } else {
      ggplot(data, aes_string(x = "size", y = "cov", color = input$color))
    }

    if (input$group != "none") {
      p = p + facet_wrap(as.formula(paste0("~", input$group)), scales = "free_x")
    }
    p = if (min_size == max_size) {
      p + geom_point(aes(x = task, y = cov))
    } else {
      p + geom_line()
    }
    p = p +
      geom_hline(yintercept = 0.95, color = "red") +
      labs(
        y = paste0("Coverage for ", input$target),
        x = "Dataset Size"
      )
    return(p)
  }
}

# primary view: learner
specifications_learnerplot = specification_factory("learner", "Vlearner", "Dlearner", "Plearner")
make_learnerplot = plotter_factory("learner")

# primary view: task
specifications_taskplot = specification_factory("task", "Vtask", "Dtask", "Ptask")
make_taskplot = plotter_factory("task")

# primary view: method
specifications_methodplot = specification_factory("method", "Vmethod", "Dmethod", "Pmethod")
make_methodplot = plotter_factory("method")
