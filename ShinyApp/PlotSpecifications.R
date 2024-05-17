specifications_ui <- function(id){
  ns <- NS(id)
    tabPanel("Fallback",
             sidebarLayout(
              sidebarPanel(
             fluidRow(
               column(6,
                      #helpText("Select variables for plotting:"),
                      selectInput(ns("x"), "X-Axis Variable:", choices = c("size")),
                      selectInput(ns("y"), "Y-Axis Variable:", choices = c("avg_cov_R","avg_cov_ER","avg_cov_PQ")),
                      selectInput(ns("color_fallback"), "...:", choices = c("task")),
                      selectInput(ns("method"), "...:", choices = c("method")),
                      br(),
                      actionButton("viewPlot_fallback", "View Plot"),
                      downloadButton(ns("downloadPlot_fallback"), "Download Plot")
                      )
             )),
             mainPanel(plotlyOutput(ns("fallbackplot"))))
             )
}

specifications_methodplot <- function(id){
  ns <- NS(id)
  tabPanel("Methods",
           sidebarLayout(
            sidebarPanel(
           fluidRow(
             column(6,
                    #helpText("Select variables for plotting:"),
                    selectInput(ns("x1"), "X-Axis Variable:", choices = c("size")),
                    selectInput(ns("y1"), "Y-Axis Variable:", choices = c("cov_R","cov_ER","cov_PQ")),
                    selectInput(ns("color_method"), "...:", choices = c("task")),
                    selectInput(ns("methodOI"), "...:", choices = levels(as.factor(as.data.frame(aggrs_base)$method))),
             br(),
             actionButton("viewPlot_method", "View Plot"),
             downloadButton(ns("downloadPlot_method"), "Download Plot")
             )
             )),
           mainPanel(plotlyOutput(ns("methodplot"))))
  )
}

specifications_download <- function(id){
      ns <- NS(id)
    tabPanel("Download specifications",
             sidebarLayout(
               sidebarPanel(
                            fluidRow(
                              column(6,selectInput(ns("units"), "Units:", choices = c("in", "cm", "mm", "px")),
                                     textInput(ns("width"), "Width:", value = 4),
                                     textInput(ns("height"), "Height:", value = 4)
                                     ))),
               mainPanel(
                 textAreaInput(ns("code"), "Enter code to modify ggplot:", rows = 5),
                 actionButton(ns("apply"), "Apply Code")
               ))
             )

    
}



makeplot <- function(clicker,clicked,plot){
  if(is.null(clicker)){ggplot()+theme_minimal()}else{
    if(clicker!=clicked){ggplot()+theme_minimal()
    }else{
      plot
    }}
}




