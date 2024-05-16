specifications_ui <- function(id){
  ns <- NS(id)
    tabPanel("Fallback",
             fluidRow(
               column(6,
                      helpText("Select variables for plotting:"),
                      selectInput(ns("x"), "X-Axis Variable:", choices = c("size")),
                      selectInput(ns("y"), "Y-Axis Variable:", choices = c("avg_cov_R")),
                      selectInput(ns("color"), "...:", choices = c("task")),
                      selectInput(ns("method"), "...:", choices = c("method"))))
             )
}
specifications_download <- function(id){
      ns <- NS(id)
    tabPanel("Download",
                            fluidRow(
                              column(6,selectInput(ns("units"), "Units:", choices = c("in", "cm", "mm", "px")),
                                     textInput(ns("width"), "Width:", value = 4),
                                     textInput(ns("height"), "Height:", value = 4),
                                     downloadButton(ns("downloadPlot"), "Download Plot"))))
    
}

plotting_ui <- function(id){
  ns <- NS(id)
  plotlyOutput(ns("plot"))
}



