specifications_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Fallback",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            6,
            # helpText("Select variables for plotting:"),
            selectInput(ns("x"), "X-Axis Variable:", choices = c("size")),
            selectInput(ns("y"), "Y-Axis Variable:", choices = c("avg_cov_R", "avg_cov_ER", "avg_cov_PQ")),
            selectInput(ns("color_fallback"), "...:", choices = c("task")),
            selectInput(ns("method"), "...:", choices = c("method")),
            br(),
            actionButton("viewPlot_fallback", "View Plot"),
            downloadButton(ns("downloadPlot_fallback"), "Download Plot")
          )
        )
      ),
      mainPanel(
        div(
          class = "plot-container",
          plotlyOutput(ns("fallbackplot"),
            height = "800px"
          ) ## we can define this + width as needed!
          # also as a function of inputs!
        )
      )
    )
  )
}

specifications_methodplot <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Method",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            6,
            # helpText("Select variables for plotting:"),
            selectInput(ns("x1"), "X-Axis Variable:", choices = c("size")),
            selectInput(ns("y1"), "Y-Axis Variable:", choices = c("cov_R", "cov_ER", "cov_PQ")),
            selectInput(ns("color_method"), "...:", choices = c("task")),
            selectInput(ns("methodOI"), "...:", choices = levels(as.factor(as.data.frame(aggrs_base)$method))),
            br(),
            actionButton("viewPlot_method", "View Plot"),
            downloadButton(ns("downloadPlot_method"), "Download Plot")
          )
        )
      ),
      mainPanel(
        div(
          class = "plot-container",
          plotlyOutput(ns("methodplot")) ## we can define height + width as needed!
          # also as a function of inputs!
        )
      )
    )
  )
}

specifications_download <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Download specifications",
    fluidRow(
      column(
        6,
        fluidRow(
          selectInput(ns("units"), "Units:", choices = c("in", "cm", "mm", "px")),
          textInput(ns("width"), "Width:", value = 4),
          textInput(ns("height"), "Height:", value = 4)
        )
      ),
      column(
        6,
        fluidRow(
          selectInput(ns("tbd1"), "TBD1:", choices = c("in", "cm", "mm", "px")),
          textInput(ns("tbd2"), "TBD2:", value = 4),
          textInput(ns("tbd3"), "TBD3:", value = 4)
        )
      )
    ),
    mainPanel(
      div(
        class = "content",
        textAreaInput(ns("code"), "Enter code to modify ggplot:", rows = 5),
        actionButton(ns("apply"), "Apply Code")
      )
    )
  )
}

# specifications_learnerplot = function(id) {
#   ns = NS(id)
#   tabPanel("Learner",
#     sidebarLayout(
#       sidebarPanel(
#         fluidRow(
#           column(6,
#             # helpText("Select variables for plotting:"),
#             selectInput(ns("min_size"), "Min Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "100"),
#             selectInput(ns("max_size"), "Max Size:", choices = as.character(c(100L, 500L, 1000L, 5000L, 10000L)), "10000"),
#             selectInput(ns("target"), "Target Quantity:", choices = c("Risk", "Expected Risk", "Proxy Quantity"), selected = "Risk"),
#             selectInput(ns("group"), "Group By:", choices = c("learner", "method", "none"), "task"),
#             selectInput(ns("color"), "Color:", choices = c("task", "method", "none"), "none"),
#             selectInput(ns("learner"), "Learner:", choices = c("linear", "ridge", "ridge_tuned", "ranger", "rpart")),
#             br(),
#             actionButton("viewPlot_learner", "View Plot"),
#             downloadButton(ns("downloadPlot_learner"), "Download Plot")
#           )
#       )),
#       mainPanel(
#         div(class = "plot-container",
#           plotlyOutput(ns("learnerplot")) ## we can define height + width as needed!
#           # also as a function of inputs!
#         )
#     ))
#   )
# }



# creates a plot specification
specification_factory <- function(atom) {
  stopifnot(atom %in% c("learner", "method", "task"))
  choices <- c("learner", "method", "task", "none")
  choices <- choices[choices != atom]

  browser()
  atom_choices <- ATOM_CHOICES[[atom]]

  function(id) {
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
                actionButton(paste0("viewPlot_", atom), "View Plot"),
                downloadButton(ns(paste0("downloadPlot_", atom)), "Download Plot")
              )
            )
          ),
          mainPanel(
            div(
              class = "plot-container",
              plotlyOutput(ns(paste0(atom, "plot"))) ## we can define height + width as needed!
              # also as a function of inputs!
            )
          )
        )
      )
    )
  }
}

plotter_factory <- function(atom) {
  function(data, input) {
    min_size <- as.integer(input$min_size)
    max_size <- as.integer(input$max_size)
    target <- translate_target(input$target)
    by_vars <- c("size", if (input$group != "none") input$group, if (input$color != "none") input$color)


    print(data[[atom]])
    print(input$atom)

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
    print(data[[input$group]])
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
specifications_learnerplot <- specification_factory("learner")
make_learnerplot <- plotter_factory("learner")

# primary view: task
specifications_taskplot <- specification_factory("task")
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

makeplot <- function(clicker, clicked, plot) {
  if (is.null(clicker)) {
    ggplot() +
      theme_minimal()
  } else {
    if (clicker != clicked) {
      ggplot() +
        theme_minimal()
    } else {
      plot
    }
  }
}
