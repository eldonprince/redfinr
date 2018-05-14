library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(ggmap)
library(ggrepel)

rdata <- read_csv("redfin_2018-05-12-18-21-04.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Home Analyzer"),
  skin = "red",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load Redfin Data", tabName = "data_load", icon = icon("upload")),
      menuItem("Home Analyzer", tabName = "analyzer", icon = icon("map-marker"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_load",
              fluidRow(
                box(title = "Instructions", 
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
                box(title = "Upload Data from Redfin",
                    fileInput("rfile", "Choose Redfin CSV file",
                             accept = c(".csv")),
                    dataTableOutput("rfile_data", width = "100%")
                    )
              )
      ),
      tabItem(tabName = "analyzer",
              plotOutput("plot_map", width = "100%", height = "1200px")
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
  
  output$rfile_data <- renderDataTable({
    dat()
  },
  options = list(scrollX = TRUE)
  )
  
  output$plot_map <- renderPlot({
    withProgress(message = "fetching map...", value = 0.75, {
      dats <- dat()
      left <- min(dats$longitude)
      right <- max(dats$longitude)
      bottom <- min(dats$latitude)
      top <- max(dats$latitude)
      map_dimensions <- c(left = left, bottom = bottom, right = right, top = top)
      map <- get_map(map_dimensions, maptype = "toner", source = "stamen")
    })
    ggmap(map) +
      #stat_density_2d(data = dats, aes(longitude, latitude, fill = ..level..), geom = "polygon", alpha = 0.3, color = NA) +
      geom_label_repel(data = dats, aes(longitude, latitude, fill = price, label = round(price / 1000))) +
      scale_fill_continuous(low = "#e5f5e0", high = "#31a354") 
  })
  
}

shinyApp(ui, server)