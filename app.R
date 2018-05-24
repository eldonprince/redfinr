library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(DT)
library(ggmap)
library(ggrepel)
library(leaflet)

# For development purposes
# rdata <- read_csv("redfin_2018-05-19-15-27-39.csv")
# names(rdat) <- c("sale_type", "sold_date", "property_type", "address", "city", 
#                  "state", "zip", "price", "beds", "baths", "location", 
#                  "square_feet", "lot_size", "year_built", "days_on_market", 
#                  "dollar_per_sqft", "hoa_per_month", "status", 
#                  "next_open_house_start", "next_open_house_end", "url", 
#                  "source", "mls_nbr", "favorite", "interested", "latitude", 
#                  "longitude")

ui <- dashboardPage(
  dashboardHeader(title = "Home Analyzer"),
  skin = "green",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load Redfin Data", tabName = "data_load", icon = icon("upload")),
      menuItem("Home Analyzer", tabName = "analyzer", icon = icon("map-marker")),
      actionButton("update", "Update Plot"),
      uiOutput("price_ui"),
      uiOutput("dollar_per_sqft_ui"),
      uiOutput("sqft_ui"),
      uiOutput("sale_type_ui"),
      uiOutput("property_type_ui")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_load",
              fluidRow(
                box(title = "Instructions", width = 4, 
                    tags$ul(
                      tags$li("Go to Redfin.com"),
                      tags$li("Filter to no more than 350 homes"),
                      tags$li("Click Download All 
                              (see screenshot below)"),
                      tags$li("Save CSV file to your computer"),
                      tags$li("Upload CSV file here")),
                    img(src = "redfin_download_all.png", 
                        align = "center", width = "100%")
                    ),
                box(title = "Upload Data from Redfin", width = 8,
                    fileInput("rfile", "Choose Redfin CSV file",
                             accept = c(".csv")),
                    dataTableOutput("rfile_data", width = "100%")
                    )
              )
      ),
      tabItem(tabName = "analyzer",
              leafletOutput("plot_map", width = "100%", height = "1200px")
      )
    )
  )
)

server <- function(input, output) {
  
  dat <- reactive({
    rfile <- input$rfile
    if (is.null(rfile)) return(NULL)
    rdat <- read_csv(rfile$datapath)
    names(rdat) <- c("sale_type", "sold_date", "property_type", "address", "city", 
                      "state", "zip", "price", "beds", "baths", "location", 
                      "square_feet", "lot_size", "year_built", "days_on_market", 
                      "dollar_per_sqft", "hoa_per_month", "status", 
                      "next_open_house_start", "next_open_house_end", "url", 
                      "source", "mls_nbr", "favorite", "interested", "latitude", 
                      "longitude")
    rdat
  })
  
  output$price_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    sliderInput("price", "Price",
                min = min(dat()[["price"]], na.rm = TRUE),
                max = max(dat()[["price"]], na.rm = TRUE),
                value = c(min(dat()[["price"]], na.rm = TRUE), max(dat()[["price"]], na.rm = TRUE)),
                step = 5000)
  })
  
  output$dollar_per_sqft_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    sliderInput("dollar_per_sqft", "$ SQFT",
                min = min(dat()[["dollar_per_sqft"]], na.rm = TRUE),
                max = max(dat()[["dollar_per_sqft"]], na.rm = TRUE),
                value = c(min(dat()[["dollar_per_sqft"]], na.rm = TRUE), max(dat()[["dollar_per_sqft"]], na.rm = TRUE)),
                step = 5)
  })
  
  output$sqft_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    sliderInput("square_feet", "SQFT",
                min = min(dat()[["square_feet"]], na.rm = TRUE),
                max = max(dat()[["square_feet"]], na.rm = TRUE),
                value = c(min(dat()[["square_feet"]], na.rm = TRUE), max(dat()[["square_feet"]], na.rm = TRUE)),
                step = 50)
  })
  
  output$sale_type_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    selectizeInput("sale_type", "Sale Type", 
                   choices = unique(dat()[["sale_type"]]),
                   selected = unique(dat()[["sale_type"]]), 
                   multiple = TRUE)
  })
  
  output$property_type_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    selectizeInput("property_type", "Property Type", 
                   choices = unique(dat()[["property_type"]]),
                   selected = unique(dat()[["property_type"]]), 
                   multiple = TRUE)
  })
  
  output$rfile_data <- renderDataTable({
    dat()
  },
  options = list(scrollX = TRUE)
  )
  
  fetch_map <- reactive({
    withProgress(message = "fetching map...", value = 0.75, {
      dats <- dat()
      left <- min(dats$longitude)
      right <- max(dats$longitude)
      bottom <- min(dats$latitude)
      top <- max(dats$latitude)
      map_dimensions <- c(left = left, bottom = bottom, right = right, top = top)
      # map <- get_map(map_dimensions, maptype = "toner", source = "stamen")
      map <- get_map(map_dimensions)
    })
    map
  })
  
  observeEvent(input$update, {
    
    dats <- dat()
    dats <- dats %>% filter(price >= input$price[1] & price <= input$price[2])
    dats <- dats %>% filter(dollar_per_sqft >= input$dollar_per_sqft[1] & dollar_per_sqft <= input$dollar_per_sqft[2])
    dats <- dats %>% filter(square_feet >= input$square_feet[1] & square_feet <= input$square_feet[2])
    dats <- dats %>% filter(sale_type %in% input$sale_type)
    dats <- dats %>% filter(property_type %in% input$property_type)
    dats
    
    output$plot_map <- renderLeaflet({
      if (is.null(dat())) return(NULL)
      pal <- colorBin(c("#a1d99b", "#74c476", "#31a354", "#006d2c"), 
                      domain = c(min(dats$price), max(dats$price)), bins = 4)
      m <- leaflet(data = dats) %>%
        addProviderTiles(providers$Stamen) %>% 
        addCircleMarkers(lng = dats$longitude, lat = dats$latitude, 
                         label = paste0(round(dats$price/1000), "K"), 
                         stroke = FALSE, radius = 7, fillOpacity = 0.8,
                         color = ~pal(dats$price))
      m 
    })
  })
}

shinyApp(ui, server)