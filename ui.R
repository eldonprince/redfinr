
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(
  pageWithSidebar(
  
  # Application title
  headerPanel("Visualize Redfin Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c('.csv')
      )
    )
  )
)
