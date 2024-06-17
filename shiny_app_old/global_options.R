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
    div(
      class = "content",
      fluidPage(
        withMathJax(),
        titlePanel("Global Options"),
        fluidRow(hr(""),
            HTML(paste(readLines("HTMLS/global_options.html"), collapse = "")),
        hr("")),
    fluidRow(
      column(
        6,
        fluidRow(
          pickerInput(ns("dgps_global"), "DGPs:",
          choices = DGPS,
          multiple = TRUE,
          setdiff(DGPS, "chen_10_null")),
          pickerInput(
            ns("methods_global"), "Inference Methods:",
            choices = METHODS,
            multiple = TRUE,
            options = list(`actions-box` = TRUE),
            selected = DEFAULT_METHODS)
          )
      )
    ))
  )
)} 
