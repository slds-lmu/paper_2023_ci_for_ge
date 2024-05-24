inputModuleServer <- function(id,vals) {
  moduleServer(id, function(input, output, session) {
    output_list <- lapply(vals, function(val) eval(parse(text=paste0("reactive(input$",val,")"))))
    names(output_list) <- vals
    return(output_list)
  })
}


specifications_DataOps = function(id) {
  ns = NS(id)
  tabPanel(
    "Global Options",
      fluidPage(
        withMathJax(),
        titlePanel("Global Options"),
        fluidRow(hr(""),
                 HTML("Explain some stuff?"),
        hr("")),
    fluidRow(
      column(
        6,
        fluidRow(
          pickerInput(ns("tasks_global"), "Selection of DGPs:", choices = DGPS),
        )
      ),
      column(
        6,
        fluidRow(
          selectInput(ns("tbd01"), "TBD1:", choices = c("in", "cm", "mm", "px")),
          textInput(ns("tbd02"), "TBD2:", value = 4),
          textInput(ns("tbd3"), "TBD3:", value = 4)
        )
      )
    )
      )
  )

}