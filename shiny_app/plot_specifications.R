theme_set(theme_bw())


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
        actionButton("apply", "Apply Code")
      )
    )
  )
}

download_vals <- list(" global_units",
                      " global_width",
                      " global_height",
                      " global_code")

setNULL <- function(download_vals){
  for(i in seq_along(download_vals)){
    assign(download_vals[[i]],NULL,env=.GlobalEnv)
  }
}





