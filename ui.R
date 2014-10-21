
library(shiny)
library(rCharts)
library(rMaps)

shinyUI(bootstrapPage(
  headerPanel("A Map of Belgium"),
  
  mapOutput("map")
))
