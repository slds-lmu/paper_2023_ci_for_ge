# creates a plot specification
specification_factory <- function(atom,view_name,download_name,plot_name) {
  stopifnot(atom %in% c("learner", "method", "task"))
  choices <- c("learner", "method", "task", "none")
  choices <- choices[choices != atom]
  
  
  atom_choices <- ATOM_CHOICES[[atom]]
  
  f<-function(id) {
    ns <- NS(id)
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

plotter_factory <- function(atom) {
  function(data, input) {
    min_size <- as.integer(input$min_size)
    max_size <- as.integer(input$max_size)
    target <- translate_target(input$target)
    by_vars <- c("size", if (input$group != "none") input$group, if (input$color != "none") input$color)
    
    
    #print(data[[atom]])
    #print(input$atom)
    
    data <- data[
      data[[atom]] == input$atom & size >= min_size & size <= max_size,
      list(cov = mean(get(paste0("cov_", target)))),
      by = by_vars
    ]
    
    p <- if (input$color == "none") {
      ggplot(data, aes_string(x = "size", y = "cov"))
    } else {
      ggplot(data, aes_string(x = "size", y = "cov", color = input$color))
    }
    #print(data[[input$group]])
    if (input$group != "none") {
      p <- p + facet_wrap(as.formula(paste0("~", input$group)), scales = "free_x")
    }
    p <- p +
      geom_line() +
      geom_hline(yintercept = 0.95, color = "red") +
      labs(
        y = paste0("Coverage for ", input$target),
        x = "Dataset Size"
      )
    return(p)
  }
}

# primary view: learner
specifications_learnerplot <- specification_factory("learner","Vlearner","Dlearner","Plearner")
make_learnerplot <- plotter_factory("learner")

# primary view: task
specifications_taskplot <- specification_factory("task","Vtask","Dtask","Ptask")
make_taskplot <- plotter_factory("task")

# primary view: task
specifications_methodplot <- specification_factory("method")
make_methodplot <- plotter_factory("method")

# primary view: task
# specifications_taskplot = specification_factory("task")
# make_taskplot = plotter_factory("task")

# make_learnerplot = function(data, input) {
#   min_size = as.integer(input$min_size)
#   max_size = as.integer(input$max_size)
#   target = translate_target(input$target)

#   by_vars = c(
#     "size",
#     if (input$group != "none") input$group,
#     if (input$color != "none") input$color
#   )

#   data = data[
#     learner == input$learner & size >= min_size & size <= max_size,
#     list(cov = mean(get(paste0("cov_", target)))),
#     by = by_vars
#   ]


#   p = if (input$color == "none") {
#     ggplot(data, aes_string(x = "size", y = "cov"))
#   } else {
#     ggplot(data, aes_string(x = "size", y = "cov", color = input$color))
#   }

#   if (input$group != "none") {
#     p = p + facet_wrap(as.formula(paste0("~", input$group)), scales = "free_x")
#   }


#   p = p +
#     geom_line() +
#     geom_hline(yintercept = 0.95, color = "red") +
#     labs(
#       y = paste0("Coverage for ", input$target),
#       x = "Dataset Size"
#     )

#   return(p)
# }


