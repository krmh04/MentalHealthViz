
library(shiny)
library(bs4Dash)
library(leaflet)
library(dplyr)
library(readxl)
library(sf)
library(tidyverse)
library(tidygeocoder)
library(plotly)
library(RJSONIO)
library(leaflet.minicharts)
#Visualization code
glp <- read.csv("F:/PE & RE Electives Semester-3/MentalHealthViz/Main_GLP.csv",encoding="UTF-8")
shinyApp(
  ui = dashboardPage(
    title = "MentalHealthViz",
    header = dashboardHeader(title = "MentalHealthViz"),
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
        menuItem("Mental Health Indicators", tabName = "ordinal", icon = icon("chart-column")),
        menuItem("Population Pyramid", tabName = "pyramid", icon = icon("cubes"))
        # menuItem("Stacked Bar Chart", tabName = "barchart", icon = icon("chart-bar")),
        # menuItem("Others", tabName = "others", icon = icon("arrow-up-right-dots"))
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
            lead = "MentalHealthViz is a visual-analytics tool to explore large-scale population survey data,with a wide range of customisability allowing the user to upload their file of choice and view different kind of visualizations",
            
            btnName = "Github Repo",
            href ="https://github.com/krmh04/MentalHealthViz",
            class = "custom-jumbotron"
          )
        ),
        tabItem(
          
          tabName = "about",
          h1("About the tool"),  
          "The focus is to investigate spatial as well as non-spatial analysis of co-occurrence of different mental health  conditions. 
          The expected outcome, a visual analytics tool, is designed to explore different sections of the 
          data available about different mental health indicators.We have visualizations with maps, simple and stacked bar charts, scatter plots, population pyramids, and others."
        ),
        tabItem(
          tabName = "samples",
          fluidRow(
            column(9,
                   leafletOutput("map", height = 600)
            )
          )
        ),
        tabItem(
          tabName = "ordinal",
          h4("What did people say when they were asked if they had any physical or mental health conditions or illnesses lasting or expected to last 12 months or more?"),
          fluidRow(
            column(3,
                   selectInput("parameter_mhyn", "Select Parameter:",
                               choices = c("Yes", "No","Don't know / Prefer not to say"),
                               selected = "Yes")
            ),
            
            column(9,plotlyOutput("plot_yn")
            ),
            
          ),  
          sidebarPanel(
            checkboxGroupInput("conditions", "Select conditions:",
                               choices = c("Anxiety", "Depression", "Loneliness"),
                               selected = "Anxiety")
          ),
          mainPanel(
            plotlyOutput("plot_mentalhealth",width = 1164, height = 416)
          )
        ),
        tabItem(
          tabName = "pyramid",
          # Dropdown to select the type of population pyramid
          fluidRow(
            column(width = 6,
                   # Dropdown to select the type of population pyramid
                   selectInput("pyramid_type", "Select Population Pyramid", 
                               choices = c("Pop Pyramid for Respondents", 
                                           "Pop Pyramid who experienced a MH Condition"))
            ),
            column(width = 4,
                   # Country dropdown menu
                   uiOutput("country_ui")
            )),
          # Plot output based on the selection
          uiOutput("dynamic_plots")
        )
      )
    )
  ),
  server = function(input, output, session) {
    # Load the data
    countries <- read_sf('F:/PE & RE Electives Semester-3/MentalHealthViz/Shapefiles/copy_4.shp',options = "ENCODING=WINDOWS-1252")
    countries <- countries %>% rename(Country = NAME_LONG)
    
    glp_geocoded <- reactive({
      glp %>% geocode(Country, method = 'osm', lat = latitude, long = longitude)
      
    })
    
    glp_total <- glp %>% 
      group_by(Country) %>%
      summarize(count = n())
    
    glp_total <- glp_total %>%
      mutate(Country = recode(Country,
                              "DRC" = "Democratic Republic of the Congo",
                              "Ivory Coast" = "Côte d'Ivoire",
                              "KSA" = "Saudi Arabia",
                              "Russia" = "Russian Federation",
                              "South Korea" = "Republic of Korea",
                              "UAE" = "United Arab Emirates",
                              "UK" = "United Kingdom",
                              "US" = "United States"))
    countries <- merge(countries, glp_total, by = 'Country', all.x = F)
     
    costBins <- c(500, 600,800, 1000,1100, 1200, Inf)
    paletteBinned <- colorBin('OrRd', domain = countries$count, bins = costBins)
    
    
    new_df <- reactive({
      glp_geocoded() %>% group_by(Country, Gender, latitude, longitude) %>% summarize(count = n())
    })
    
    gender_df <- reactive({
      new_df() %>% group_by(Country) %>% pivot_wider(names_from = Gender, values_from = count, values_fill = 0)
    })
    
    colors <- c("#dd22dd", "#276f87","#c8F213","#808080")
    output$map <- renderLeaflet({
      gender_data <- gender_df()
      leaflet(gender_data) %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>% setView(lng = 78.9629, lat = 20.5937, zoom = 5) %>%
        
        addPolygons(data = countries,
                    # soften the weight of the state borders
                    weight = 1,
                    
                    # values >1 simplify the polygons' lines for less detail but faster loading
                    smoothFactor = .3,
                    
                    # set opacity of polygons
                    fillOpacity = .75,
                    
                    # specify that the each state should be colored per paletteNum()
                    fillColor = ~paletteBinned(countries$count)) %>% addLegend(pal = paletteBinned, values = countries$count,
                                                                             title = '<small>No of particiipants</small>',
                                                                             position = 'bottomleft') %>% 
        addMinicharts(gender_data$longitude, gender_data$latitude, type = "pie", chartdata = gender_data[,c("Female", "Male", "Other","Don't know/ Prefer not to say")], colorPalette = colors, width = (gender_data$Male +gender_data$Female)/100, transitionTime = 0)
    })
    
    mh_yn <- glp %>% group_by(Country, I13) %>% summarize(count = n()) %>% pivot_wider(names_from = I13, values_from = count)
    glp_indicators <- glp %>% rename(anxiety = 'C1_6', depression = 'C1_4', loneliness = 'C1_1') %>%
      mutate(anxiety_depression = ifelse(anxiety == "Yes" & depression == "Yes", "Yes", "No"),
             anxiety_loneliness = ifelse(anxiety == "Yes" & loneliness == "Yes", "Yes", "No"),
             depression_loneliness = ifelse(depression == "Yes" & loneliness == "Yes", "Yes", "No"),
             all_three = ifelse(anxiety == "Yes" & depression == "Yes" & loneliness == "Yes", "Yes", "No")) %>%
      select(Country, anxiety, depression, loneliness, anxiety_depression, anxiety_loneliness, depression_loneliness, all_three)
    
    glp_summary <- glp_indicators %>% group_by(Country) %>% summarise(across(anxiety:all_three, ~ sum(. == "Yes"), .names = "count_{col}"))
    
   # countries <- jsonlite::fromJSON(txt ="F:/PE & RE Electives Semester-3/MentalHealthViz/Shapefiles/World_Countries_Generalized.json")
    
    output$plot_yn <- renderPlotly({
      param_mhyn <- input$parameter_mhyn
      plot_ly(data = mh_yn, type = "choropleth", locations = ~Country, z = ~get(param_mhyn), colorscale = "Blues", locationmode = "country names") %>%
        colorbar(title = "No of answers") %>%
        layout(geo = list(scope = "world"), title = paste("Mental Health Indicators -", param_mhyn))
    })
    
    selected_column <- reactive({
      if (all(c("Anxiety", "Depression", "Loneliness") %in% input$conditions)) {
        "count_all_three"
      } else if (all(c("Anxiety", "Depression") %in% input$conditions)) {
        "count_anxiety_depression"
      } else if (all(c("Anxiety", "Loneliness") %in% input$conditions)) {
        "count_anxiety_loneliness"
      } else if (all(c("Depression", "Loneliness") %in% input$conditions)) {
        "count_depression_loneliness"
      } else if ("Anxiety" %in% input$conditions) {
        "count_anxiety"
      } else if ("Depression" %in% input$conditions) {
        "count_depression"
      } else if ("Loneliness" %in% input$conditions) {
        "count_loneliness"
      }
    })
    
    output$plot_mentalhealth <- renderPlotly({
      plot_ly(data = glp_summary, type = "choropleth", geojson = countries, locations = ~Country, z = ~get(selected_column()), colorscale = "Redor", locationmode = "country names") %>%
        colorbar(title = "No of responses") %>%
        layout(geo = list(scope = "world"), title = "Prevealance of Mental Health conditions")
    })
    
    

    
    
      
    filtered_data <- reactive({
      glp %>% 
        filter(Country == input$`pyramid-dropdown`) %>%  
        filter(Gender %in% c("Female", "Male")) %>%   
        group_by(Gender, Age_Group) %>%
        summarize(count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = Gender, values_from = count) %>%
        mutate(Male = -Male)  # Make male counts negative
    })
    output$country_ui <- renderUI({
      if (input$pyramid_type != "") {
        selectInput("pyramid-dropdown", "Country", choices = sort(unique(glp$Country)), selected = "Argentina")
        
      }
      
    })
    
    filtered_data_mental_health<-reactive({
      glp %>% rename(anxiety = 'C1_6', depression = 'C1_4', loneliness = 'C1_1')%>%  
        filter(Country == input$`pyramid-dropdown`) %>%  
        filter(Gender %in% c("Female", "Male")) %>%   
        group_by(Gender, Age_Group) %>%
        summarise(across(anxiety:loneliness, ~ sum(. == "Yes"), .names = "count_{col}")) |> 
        pivot_wider(names_from = Gender, values_from = c(count_anxiety,count_depression, count_loneliness))  %>%
        mutate(count_anxiety_Male  = -count_anxiety_Male,count_depression_Male=-count_depression_Male,count_loneliness_Male=-count_loneliness_Male)  # Make male counts negative
    })
 
    output$dynamic_plots <- renderUI({
      if (input$pyramid_type == "Pop Pyramid for Respondents") {
        # Return only one plot for the first option
        div(plotlyOutput("pyramid_chart"),align="center")
      } else {
        tagList(
          fluidRow(
            column(
              width = 6,  # Adjust width as necessary
              selectInput("condition-dropdown", "Condition", 
                          choices = c("Anxiety", "Depression", "Loneliness"), 
                          selected = "Anxiety")
            ),
            column(
              width = 6,  # Adjust width as necessary
              selectInput("value-type-dropdown", "Value Type", 
                          choices = c("Absolute Values", "Percentage"), 
                          selected = "Absolute Values")
            )
          ),
          
          # Conditional rendering of plots based on the selected condition and value type
          conditionalPanel(
            condition = "input['condition-dropdown'] == 'Anxiety' && input['value-type-dropdown'] == 'Absolute Values'",
            div(plotlyOutput("anxiety_plot"),align="center")
          ),
          conditionalPanel(
            condition = "input['condition-dropdown'] == 'Anxiety' && input['value-type-dropdown'] == 'Percentage'",
            div(plotlyOutput("anxiety_per_plot"),align="center")
          ),
          conditionalPanel(
            condition = "input['condition-dropdown'] == 'Depression' && input['value-type-dropdown'] == 'Absolute Values'",
            div(plotlyOutput("depression_plot"),align="center")
          ),
          conditionalPanel(
            condition = "input['condition-dropdown'] == 'Depression' && input['value-type-dropdown'] == 'Percentage'",
            div(plotlyOutput("depression_per_plot"),align="center")
          ),
          conditionalPanel(
            condition = "input['condition-dropdown'] == 'Loneliness' && input['value-type-dropdown'] == 'Absolute Values'",
            div(plotlyOutput("loneliness_plot"),align="center")
          ),
          conditionalPanel(
            condition = "input['condition-dropdown'] == 'Loneliness' && input['value-type-dropdown'] == 'Percentage'",
            div(plotlyOutput("loneliness_per_plot"),align="center")
          )
        )
      }
    })
    
    output$pyramid_chart <- renderPlotly({
      
      pyramid_df <- filtered_data()
      
      selected_country <- input$`pyramid-dropdown`
      
      plot_ly(pyramid_df) %>%
        add_trace(
          x = ~Male, y = ~Age_Group, 
          type = 'bar', name = 'Male',
          marker = list(color = '#303f70'),
          orientation = 'h'
        ) %>%
        add_trace(
          x = ~Female, y = ~Age_Group, 
          type = 'bar', name = 'Female',
          marker = list(color = '#bd5cb0'),
          orientation = 'h'
        ) %>%
        layout(
          title = paste("Population Pyramid for", selected_country),
          yaxis = list(title = "Age Group"),
          xaxis = list(title= 'Count of participants', tickmode = 'array', tickvals = c(-150, -100, -50, 0, 50, 100, 150),
                       ticktext = c('150', '100', '50', '0', '50', '100', '150')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    #for anxiety
    output$anxiety_plot <- renderPlotly({
      
      pyramid_df_anxiety <- filtered_data_mental_health()
      
      selected_country <- input$`pyramid-dropdown`
      
      plot_ly(pyramid_df_anxiety) %>%
        add_trace(
          x = ~count_anxiety_Male, y = ~Age_Group, 
          type = 'bar', name = 'Male',
          marker = list(color = '#303f70'),
          orientation = 'h'
        ) %>%
        add_trace(
          x = ~count_anxiety_Female, y = ~Age_Group, 
          type = 'bar', name = 'Female',
          marker = list(color = '#bd5cb0'),
          orientation = 'h'
        ) %>%
        layout(
          title = paste("Population Pyramid-Anxiety for", selected_country),
          yaxis = list(title = "Age Group"),
          xaxis = list(title= 'Count of participants', tickmode = 'array', tickvals = c(-100, -80, -60,-40,-20, 0, 20, 40, 60,80,100),
                       ticktext = c('100', '80', '60','40','20', '0', '20', '40', '60','80','100')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    
    #for depression
    
    output$depression_plot <- renderPlotly({
      
      pyramid_df_depression <- filtered_data_mental_health()
      
      selected_country <- input$`pyramid-dropdown`
      
      plot_ly(pyramid_df_depression) %>%
        add_trace(
          x = ~count_depression_Male, y = ~Age_Group, 
          type = 'bar', name = 'Male',
          marker = list(color = '#303f70'),
          orientation = 'h'
        ) %>%
        add_trace(
          x = ~count_depression_Female, y = ~Age_Group, 
          type = 'bar', name = 'Female',
          marker = list(color = '#bd5cb0'),
          orientation = 'h'
        ) %>%
        layout(
          title = paste("Population Pyramid-Depression for", selected_country),
          yaxis = list(title = "Age Group"),
          xaxis = list(title= 'Count of participants', tickmode = 'array', tickvals = c(-140,-120,-100, -80, -60,-40,-20, 0, 20, 40, 60,80,100,120,140),
                       ticktext = c('140','120','100', '80', '60','40','20', '0', '20', '40', '60','80','100','120','140')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    
    
    
    #for loneliness
    output$loneliness_plot <- renderPlotly({
      
      pyramid_df_loneliness <- filtered_data_mental_health()
      
      selected_country <- input$`pyramid-dropdown`
      
      plot_ly(pyramid_df_loneliness) %>%
        add_trace(
          x = ~count_loneliness_Male, y = ~Age_Group, 
          type = 'bar', name = 'Male',
          marker = list(color = '#303f70'),
          orientation = 'h'
        ) %>%
        add_trace(
          x = ~count_loneliness_Female, y = ~Age_Group, 
          type = 'bar', name = 'Female',
          marker = list(color = '#bd5cb0'),
          orientation = 'h'
        ) %>%
        layout(
          title = paste("Population Pyramid-Loneliness for", selected_country),
          yaxis = list(title = "Age Group"),
          xaxis = list(title= 'Count of participants', tickmode = 'array', tickvals = c(-100, -80, -60,-40,-20, 0, 20, 40, 60,80,100),
                       ticktext = c('100', '80', '60','40','20', '0', '20', '40', '60','80','100')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    })
    #FOR PERCENTAGES
    
    
    #for anxiety

    glp_total <- reactive({
      glp %>% 
        filter(Country == input$`pyramid-dropdown`) %>%
        summarize(count = n())
    })
    
    
    output$anxiety_per_plot <- renderPlotly({
      
      pyramid_df_anxiety <- filtered_data_mental_health()
      total_count <- glp_total()
      selected_country <- input$`pyramid-dropdown`
      
      plot_ly() %>%
        add_trace(
          x = (pyramid_df_anxiety$count_anxiety_Male/total_count$count)*100, y = pyramid_df_anxiety$Age_Group, 
          type = 'bar', name = 'Male',
          marker = list(color = '#303f70'),
          orientation = 'h'
        ) %>%
        add_trace(
          x = (pyramid_df_anxiety$count_anxiety_Female/total_count$count)*100, y = pyramid_df_anxiety$Age_Group, 
          type = 'bar', name = 'Female',
          marker = list(color = '#bd5cb0'),
          orientation = 'h'
        ) %>%
        layout(
          title = paste("Population Pyramid-Anxiety for", selected_country),
          yaxis = list(title = "Age Group"),
          xaxis = list(title= 'Count of participants', tickmode = 'array', tickvals = c(-100, -80, -60,-40,-20, 0, 20, 40, 60,80,100),
                       ticktext = c('100', '80', '60','40','20', '0', '20', '40', '60','80','100')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    })  
    
    
    #for loneliness
    
    output$loneliness_per_plot <- renderPlotly({
      
      pyramid_df_loneliness <- filtered_data_mental_health()
      total_count <- glp_total()
      
      selected_country <- input$`pyramid-dropdown`
      
      plot_ly() %>%
        add_trace(
          x = (pyramid_df_loneliness$count_loneliness_Male/total_count$count)*100, y = pyramid_df_loneliness$Age_Group, 
          type = 'bar', name = 'Male',
          marker = list(color = '#303f70'),
          orientation = 'h'
        ) %>%
        add_trace(
          x = (pyramid_df_loneliness$count_loneliness_Female/total_count$count)*100, y = pyramid_df_loneliness$Age_Group, 
          type = 'bar', name = 'Female',
          marker = list(color = '#bd5cb0'),
          orientation = 'h'
        ) %>%
        layout(
          title = paste("Population Pyramid-loneliness for", selected_country),
          yaxis = list(title = "Age Group"),
          xaxis = list(title= 'Count of participants', tickmode = 'array', tickvals = c(-100, -80, -60,-40,-20, 0, 20, 40, 60,80,100),
                       ticktext = c('100', '80', '60','40','20', '0', '20', '40', '60','80','100')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    }) 
    
    
    
    #for depression
    output$depression_per_plot <- renderPlotly({
      
      pyramid_df_depression <- filtered_data_mental_health()
            total_count <- glp_total()

      selected_country <- input$`pyramid-dropdown`
      
      plot_ly() %>%
        add_trace(
          x = (pyramid_df_depression$count_depression_Male/total_count$count)*100, y = pyramid_df_depression$Age_Group, 
          type = 'bar', name = 'Male',
          marker = list(color = '#303f70'),
          orientation = 'h'
        ) %>%
        add_trace(
          x = (pyramid_df_depression$count_depression_Female/total_count$count)*100, y = pyramid_df_depression$Age_Group, 
          type = 'bar', name = 'Female',
          marker = list(color = '#bd5cb0'),
          orientation = 'h'
        ) %>%
        layout(
          title = paste("Population Pyramid-depression for", selected_country),
          yaxis = list(title = "Age Group"),
          xaxis = list(title= 'Count of participants', tickmode = 'array', tickvals = c(-100, -80, -60,-40,-20, 0, 20, 40, 60,80,100),
                       ticktext = c('100', '80', '60','40','20', '0', '20', '40', '60','80','100')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    }) 
    
  })































