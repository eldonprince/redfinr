library(ggvis)
library(shiny)
library(readr)
redfin_names <- sort(names(read_csv("redfin_names.csv")))

shinyUI(
  fluidPage(
    
    titlePanel("Visualize Redfin Data"),
    fileInput('file1', 'Choose CSV file from Redfin.com',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    selectInput('x_col', label = "x-axis", choices = redfin_names),
    selectInput('y_col', label = "y-axis", choices = redfin_names),
    tabsetPanel(type = "tabs", 
                tabPanel("Plot", ggvisOutput("plot")),
                tabPanel("Map", plotOutput("map_plot")),
                tabPanel("Data", tableOutput("contents"))
    )
  )
)
