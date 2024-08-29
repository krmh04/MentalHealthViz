library(shiny)
library(bs4Dash)
library(leaflet)
library(dplyr)
library(readxl)

# Load the data
nfhs5_state <- read_excel("sample_size_statewise.xlsx")   
nfhs5_district <- read_excel("sample_size_district.xlsx")

shinyApp(
  ui = dashboardPage(
    title = "NFHS5",
    header = dashboardHeader(title = "NFHS5"),
    footer = NULL,
    options = NULL,
    fullscreen = TRUE,
    help = TRUE,
    scrollToTop = TRUE,
    
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "sidebarMenuid",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("About", tabName = "about", icon = icon("clipboard")),
        menuItem("Population Samples", tabName = "samples", icon = icon("person")),
        menuItem("Ordinal Variables", tabName = "ordinal", icon = icon("chart-column")),
        menuItem("Population Pyramid", tabName = "pyramid", icon = icon("cubes")),
        menuItem("Stacked Bar Chart", tabName = "barchart", icon = icon("chart-bar")),
        menuItem("Others", tabName = "others", icon = icon("arrow-up-right-dots"))
      )
    ),
    body = dashboardBody(  
      tags$style(HTML("
    .custom-jumbotron {
      font-weight:bold;
      color: #000000  !important; }
      .lead{
      color: #000000 !important ;
      font-weight:400;
      }
      .jumbotron {
    background-color:#9cc1c0 !important; 
      }
      .jumbotron h1{
      color:#00098c !important;
      }
  ")),
      
      tabItems(
        tabItem(
          tabName = "home",
          jumbotron(
            title = "Welcome!",
            status = "info",
            lead = "NFHS5Vis is a visual-analytics tool to explore large-scale population survey data, with a focus on the National Family Health Survey 2019-20 in India, also referred to as NFHS-5. A visual analytics tool is where a data analytic workflow, with feedback loops in the presence of interactive visualizations, is implemented. NFHS-5 is a landmark health survey in India, where it gives estimates of most of the health as well as socio-economic indicators at the district level for the first time for all the 640 districts in India. NFHS-5 is the fifth edition of a relatively more frequent national survey conducted by the Ministry of Health and Family Welfare, Government of India, and implemented by the Indian Institute of Population Studies.",
            href = "https://dhsprogram.com/data/dataset/India_Standard-DHS_2020.cfm?flag=0",
            btnName = "Download",
            class = "custom-jumbotron",
            "Data available from the DHS program"
          )
        ),
        tabItem(
          
          tabName = "about",
          title = "About the tool!",
          
          "The focus of NFHS5Vis has been to investigate spatial as well as non-spatial analysis of co-occurrence of different malnutrition conditions. The expected outcome, a visual analytics tool, is designed to explore different sections of the data available in NFHS-5, with a scope of generalization for extensions to future editions of NFHS. Hence, NFHS5Vis in its current state inspects the variables in its raw form or as weighted aggregate to investigate spatial distribution at both national and district level. We also investigate non-spatial visualizations to give a different perspective to the data. Here, we investigate variables both in district and state levels in India. We identify variables as count data (population samples), ordinal variables, and continuous variables. 
          We have visualizations with maps, simple and stacked bar charts, scatter plots, population pyramids, and others"
        ),
        tabItem(
          tabName = "samples",
          fluidRow(
            column(3,
                   radioButtons("region_type", "Select Region Type:",
                                choices = c("State", "District"),
                                selected = "State"),
                   selectInput("parameter", "Select Parameter:",
                               choices = c("Households", "Men", "Women"),
                               selected = "Households")
            ),
            column(9,
                   leafletOutput("map", height = 600)
            )
          )
        ),
        tabItem(
          tabName = "ordinal"
        ),
        tabItem(
          tabName = "pyramid"
        ),
        tabItem(
          tabName = "barchart"
        ),
        tabItem(
          tabName = "others"
        )
      )
    )
  ),
  server = function(input, output, session) {
    
    map_data <- reactive({
      if (input$region_type == "State") {
        nfhs5_state
      } else {
        nfhs5_district
      }
    })
    
    output$map <- renderLeaflet({
      data <- map_data()
      param <- tolower(input$parameter)
      leaflet(data) %>%
        addTiles() %>%
        setView(lng = 78.9629, lat = 20.5937, zoom = 5) %>%   
        addCircleMarkers(
          lng = ~long, lat = ~lat,
          radius = ~sqrt(households) / 8,  
          color = "red",
          stroke = FALSE, fillOpacity = 0.7,
          
          label = ~paste(if(input$region_type == "State"){
            state
          }
          else{
            district
          },
          
          ": ", get(param), " ", tolower(input$parameter))
        )
    })
  }
)