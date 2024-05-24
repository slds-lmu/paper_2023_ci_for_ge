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
                selectInput(ns("min_size"), "Min Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
                selectInput(ns("max_size"), "Max Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "10000"),
                selectInput(ns("loss_regr"), "Loss(Regr):", LOSSES$regr, "Squared"),
                selectInput(ns("loss_classif"), "Loss(Classif):", LOSSES$classif, "Zero-One"),
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
  function(data, input) {
    min_size = as.integer(input$min_size)
    max_size = as.integer(input$max_size)
    target = translate_target(input$target)
    by_vars = c("size", if (input$group != "none") input$group, if (input$color != "none") input$color)

    
    min_y = input$slider1
    max_y = input$slider2


    data = data[
      data[[atom]] == input$atom & size >= min_size & size <= max_size & measure %in% translate_losses(input$loss_regr, input$loss_classif),
      list(
        cov_error = sqrt(mean((get(paste0("cov_", target)) - 0.95)^2))
      ),
      by = by_vars
    ]

    print(data)

    p = if (input$color == "none") {
      ggplot(data, aes_string(x = "size", y = "cov_error"))
    } else {
      ggplot(data, aes_string(x = "size", y = "cov_error", color = input$color))
    }

    if (input$group != "none") {
      p = p + facet_wrap(as.formula(paste0("~", input$group)), scales = "free_x")
    }
    p = p + geom_line() + ylim(min_y, max_y) +
      labs(
        y = paste0("Coverage Error (RMSE) for ", input$target),
        x = "Dataset Size"
      )
    return(p)
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
