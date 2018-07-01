library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tibble)
library(purrr)
library(leaflet)
library(googleway)
library(geosphere)

#For development purposes
# rdat <- read_csv("redfin_2018-06-30-15-19-35.csv", col_types = c("ccccccciidciiiiidccccciccdd"))
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
    includeCSS("www/theme.css"),
    sidebarMenu(
      menuItem("Load Data", tabName = "data_load", icon = icon("upload")),
      menuItem("Analyze", tabName = "analyzer", icon = icon("home")),
      br(),
      actionButton("update", "Update Map", icon = icon("map"), 
                   style = "color: #fff; background-color: #006d2c"),
      selectizeInput("homes_facing", "Homes Facing", choices = c("North", "South", "East", "West", "Unknown"),
                     selected = c("North", "South", "East", "West", "Unknown"), multiple = TRUE),
      uiOutput("price_ui"),
      uiOutput("dollar_per_sqft_ui"),
      uiOutput("sqft_ui"),
      uiOutput("lot_size_ui"),
      uiOutput("year_built_ui"),
      uiOutput("sale_type_ui"),
      uiOutput("property_type_ui"),
      br(),
      tags$a(href = "https://www.redfin.com", tags$img(src = "redfin_logo.png", width = "50%"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_load",
              fluidRow(
                box(title = "Instructions", width = 5, 
                    tags$li("Go to Redfin.com"),
                    tags$li("Filter to no more than 350 homes (however multiple Redfin files can be loaded into this app)"),
                    tags$li("Click Download All"),
                    img(src = "redfin_download_all.png", 
                        align = "center", width = "100%"),
                    br(),
                    br(),
                    tags$li("Save CSV file(s) to your computer"),
                    br(),
                    fileInput("rfile", "Choose Redfin CSV file(s)",
                              accept = c(".csv"), multiple = TRUE),
                    tags$li("Go to the Analyze menu on the sidebar and click Update Map"),
                    br()
                )
              )
      ),
      tabItem(tabName = "analyzer",
              fluidRow(
                uiOutput("plot_map_ui"),
                numericInput("plot_height", "Map height in pixels", value = 800, min = 300, max = 2400)
              )
      )
    )
  )
)

server <- function(input, output) {
  
  dat <- reactive({
    rfile <- input$rfile
    if (is.null(rfile)) return(NULL)
    rdat <- bind_rows(lapply(rfile$datapath, function(x) read_csv(x, col_types = c("ccccccciidciiiiidccccciccdd"))))
    names(rdat) <- c("sale_type", "sold_date", "property_type", "address", "city", 
                      "state", "zip", "price", "beds", "baths", "location", 
                      "square_feet", "lot_size", "year_built", "days_on_market", 
                      "dollar_per_sqft", "hoa_per_month", "status", 
                      "next_open_house_start", "next_open_house_end", "url", 
                      "source", "mls_nbr", "favorite", "interested", "latitude", 
                      "longitude")
    
    #Get home orientation
    rdat_plan <- rdat %>% filter(sale_type == "New Construction Plan" | property_type != "Single Family Residential")
    rdat_gps <- rdat
    if (nrow(rdat_plan) > 0) {
      rdat_plan$direction <- "Unknown"
      rdat_plan$bearing <- NA  
      rdat_gps <- rdat %>% filter(sale_type != "New Construction Plan" & property_type == "Single Family Residential")
    }
    
    rdat_gps$api_group <- factor(rep(c(1:ceiling(nrow(rdat_gps) / 100)), ceiling(nrow(rdat_gps) / ceiling(nrow(rdat_gps) / 100)))[1:nrow(rdat_gps)])
    rdat_list <- split(rdat_gps, rdat_gps$api_group)
    rdat_list <- lapply(rdat_list, function(x) rowid_to_column(x, "originalIndex"))
    
    nearest_road <- lapply(rdat_list, function(x) google_nearestRoads(data.frame(lat = x$latitude, lon = x$longitude), key = Sys.getenv("road_key")))
    nearest_road <- lapply(nearest_road, function(x) x$snappedPoints)
    
    joins <- function(x, y) {
      y <- data.frame(road_latitude = y$location$latitude, road_longitude = y$location$longitude, originalIndex = y$originalIndex + 1)
      y <- unique(y)
      left_join(x, y, by = "originalIndex")
    }
    rdat_gps <- bind_rows(map2(rdat_list, nearest_road, joins))
    
    rdat_gps$bearing <- bearing(data.frame(rdat_gps[["longitude"]], rdat_gps[["latitude"]]), data.frame(rdat_gps[["road_longitude"]], rdat_gps[["road_latitude"]]))
    rdat_gps <- rdat_gps %>% mutate(direction = if_else(bearing >= -45 & bearing <= 45, "North", 
                                                        if_else(bearing >= 45 & bearing <= 135, "East",
                                                                if_else(bearing >= 135 | bearing <= -135, "South", 
                                                                        if_else(bearing <= -45 & bearing >= -135, "West", "Unknown")))))
    
    if (nrow(rdat_plan) > 0) {
      rdat <- bind_rows(rdat_gps, rdat_plan)  
    }
    if (nrow(rdat_plan) == 0) {
      rdat <- rdat_gps  
    }
    
    rdat$direction <- ifelse(is.na(rdat$direction), "Unknown", rdat$direction)
    
    rdat
  })
  
  output$price_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    sliderInput("price", "Price",
                min = min(dat()[["price"]], na.rm = TRUE),
                max = max(dat()[["price"]], na.rm = TRUE),
                value = c(min(dat()[["price"]], na.rm = TRUE), max(dat()[["price"]], na.rm = TRUE)),
                step = 1000)
  })
  
  output$dollar_per_sqft_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    sliderInput("dollar_per_sqft", "$ SQFT",
                min = min(dat()[["dollar_per_sqft"]], na.rm = TRUE),
                max = max(dat()[["dollar_per_sqft"]], na.rm = TRUE),
                value = c(min(dat()[["dollar_per_sqft"]], na.rm = TRUE), max(dat()[["dollar_per_sqft"]], na.rm = TRUE)),
                step = 1)
  })
  
  output$sqft_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    sliderInput("square_feet", "SQFT",
                min = min(dat()[["square_feet"]], na.rm = TRUE),
                max = max(dat()[["square_feet"]], na.rm = TRUE),
                value = c(min(dat()[["square_feet"]], na.rm = TRUE), max(dat()[["square_feet"]], na.rm = TRUE)),
                step = 1)
  })
  
  output$lot_size_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    sliderInput("lot_size", "Lot Size",
                min = min(dat()[["lot_size"]], na.rm = TRUE),
                max = max(dat()[["lot_size"]], na.rm = TRUE),
                value = c(min(dat()[["lot_size"]], na.rm = TRUE), max(dat()[["lot_size"]], na.rm = TRUE)),
                step = 1)
  })
  
  output$year_built_ui <- renderUI({
    if (is.null(dat())) return(NULL)
    sliderInput("year_built", "Year Built",
                min = min(dat()[["year_built"]], na.rm = TRUE),
                max = max(dat()[["year_built"]], na.rm = TRUE),
                value = c(min(dat()[["year_built"]], na.rm = TRUE), max(dat()[["year_built"]], na.rm = TRUE)),
                step = 1, sep = "")
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
  
  output$plot_map_ui <- renderUI({
    leafletOutput("plot_map", width = "100%", height = paste0(input$plot_height, "px"))
  })
  
  observeEvent(input$update, {
    
    dats <- dat()
    dats <- dats %>% filter(price >= input$price[1] & price <= input$price[2])
    dats <- dats %>% filter(dollar_per_sqft >= input$dollar_per_sqft[1] & dollar_per_sqft <= input$dollar_per_sqft[2])
    dats <- dats %>% filter(square_feet >= input$square_feet[1] & square_feet <= input$square_feet[2])
    dats <- dats %>% filter(lot_size >= input$lot_size[1] & lot_size <= input$lot_size[2])
    dats <- dats %>% filter(year_built >= input$year_built[1] & year_built <= input$year_built[2])
    dats <- dats %>% filter(sale_type %in% input$sale_type)
    dats <- dats %>% filter(property_type %in% input$property_type)
    dats <- dats %>% filter(direction %in% input$homes_facing)
    dats <- dats %>% mutate(content = paste(sep = "<br/>", 
                                            paste0("<b><a href = '", url, "' target = '_blank'>", paste0(location, " ", round(price/1000), "K"), "</a></b>"),
                                            paste(beds, "beds  ", baths, "baths"),
                                            paste(square_feet, "sqft"),
                                            paste0("$", dollar_per_sqft, " / sqft")))
    
    output$plot_map <- renderLeaflet({
      if (is.null(dat())) return(NULL)
      pal <- colorBin(c("#762a83", "#1b7837"), 
                      domain = c(min(dats$price), max(dats$price)), bins = 3)
      m <- leaflet(data = dats) %>%
        addProviderTiles(providers$Stamen) %>% 
        addCircleMarkers(lng = dats$longitude, lat = dats$latitude, 
                         label = paste0(round(dats$price/1000), "K"),
                         labelOptions = labelOptions(direction = "top"),
                         popup = dats$content,
                         popupOptions = popupOptions(closeButton = FALSE),
                         stroke = FALSE, radius = 7, fillOpacity = 0.9,
                         color = ~pal(dats$price)) %>% 
        addMiniMap(tiles = providers$Stamen, toggleDisplay = TRUE) %>% 
        addLegend("bottomright", pal = pal, values = ~ dats$price,
                  title = "Home Price",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 0.8)
      m 
    })
  })
}

shinyApp(ui, server)