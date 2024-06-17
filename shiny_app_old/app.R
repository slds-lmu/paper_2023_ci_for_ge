library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(here)
library(DT)
# library(shinyjs)
library(shinyWidgets)

source("app_content.R")

# Run the application
shinyApp(ui, server)
