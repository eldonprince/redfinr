
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(readr)
library(ggvis)

shinyServer(function(input, output) {
  
  my_data <- reactive({  
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data.frame(read_csv(inFile$datapath))
  })
  
  output$plot <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dataset <- my_data()
    suppressMessages(dataset %>% ggvis(prop("x", as.name(input$x_col)), prop("y", as.name(input$y_col))) %>% 
      add_axis("x", title = ifelse(is.null(input$x_col), "x-axis title", input$x_col), title_offset = 75) %>%
      add_axis("y", title = ifelse(is.null(input$y_col), "y-axis title", input$y_col), title_offset = 75) %>%
      bind_shiny("plot"))
  })
               
  output$contents <- renderTable({
    my_data()
  })

  output$map_plot <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    dataset <- my_data()
    plot(dataset$SQFT)
  })
})

