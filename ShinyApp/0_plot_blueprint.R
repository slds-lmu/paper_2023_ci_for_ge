#### generate data you need

specifications_blueprint <- function(id){
  ns <- NS(id)
  tabPanel("Blueprint",
           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 column(6,
                        #helpText("Select variables for plotting:"),
                        ##specify inputs for plotting
                        br(),
                        actionButton("viewPlot_blueprint", "View Plot"),
                        downloadButton(ns("downloadPlot_blueprint"), "Download Plot")
                 )
               )),
             mainPanel(plotlyOutput(ns("blueprintplot"))))
  )
}

blueprint_plot <- function(inputs){
  #...
}