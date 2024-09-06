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
new_mhdata <- read_csv("F:/PE & RE Electives Semester-3/MentalHealthViz/geocoded_ProfOgunbode.csv")
names(new_mhdata) <- gsub("[\r\n]", "", names(new_mhdata))

geocoded_file_path <- "F:/PE & RE Electives Semester-3/MentalHealthViz/geocoded_glp.csv"
if (file.exists(geocoded_file_path)) {
  glp_geocoded_data <- read.csv(geocoded_file_path)
} else {
   glp_geocoded_data <- glp %>% geocode(Country, method = 'osm', lat = latitude, long = longitude)
  write.csv(glp_geocoded_data, geocoded_file_path, row.names = FALSE)
}
geocoded_data <- reactiveVal(glp_geocoded_data)

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
        menuItem("Mental Health Indicators", tabName = "mentalhealth", icon = icon("notes-medical")),
        menuItem("Ordinal data", tabName = "ordinal", icon = icon("chart-column")),
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
          tabName = "mentalhealth",
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
                               choices = c("Anxiety", "Death", "Loneliness"),
                               selected = "Anxiety")
          ),
          mainPanel(
            plotlyOutput("plot_mentalhealth",width = 1164, height = 416)
          )
        ),
        tabItem(
          tabName = "ordinal",
          # Dropdown to select the type of population pyramid
          h4("Respondents' Feelings with regards to Climate Anxiety"),
          
          fluidRow(
            column(width = 6,
                   # Dropdown to select the type of population pyramid
                   selectInput("choice_type", "Select the parameter", 
                                choices = names(new_mhdata)[4:10]) 
            ),
            column(width=6,
                   tags$style(type="text/css",
                              ".leaflet-container {height: 600px; margin: auto;}"),
                   div(style = "display: flex; align-items: center;margin-left: -152px",
                       leafletOutput("bar_map", height = 600)
                   )
            )
        )),
        
        
        
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
    
    # glp_geocoded <- reactive({
    #   glp %>% geocode(Country, method = 'osm', lat = latitude, long = longitude)
    #   
    # })
    
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
      geocoded_data () %>% group_by(Country, Gender, latitude, longitude) %>% summarize(count = n())
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
        addMinicharts(gender_data$longitude, gender_data$latitude, type = "pie", chartdata = gender_data[,c("Female", "Male", "Other","Don't know/ Prefer not to say")], colorPalette = colors, width = (gender_data$Male +gender_data$Female)/80, transitionTime = 0)
    })
    
    # colors_for_mh <- c("#3093e5", "#fcba50", "#a0d9e8","#dfdfdf","#000000")
    # 
    # 
    # reactive_mh_data <- reactive({
    #   # selected_type <- input$`choice_type`
    #   
    #   new_mhdata |>   
    #     group_by(Country_Code, input$`choice_type`, latitude, longitude) |> 
    #     summarize(count = n()) 
    #   
    # })
    # reactive_mh_data <-reactive({
    #   
    #   
    #   reactive_mh_data |> pivot_wider(names_from = input$`choice_type`, values_from = count, values_fill = 0)
    # })
    # # 
    # # new_mhdata <- new_mhdata |>   
    # #   group_by(Country_Code, selected_type,latitude,longitude) |> 
    # #   summarize(count = n())
    # # 
    # 
    # # new_mhdata<-  new_mhdata |>  pivot_wider(names_from = selected_type, values_from = count, values_fill = 0)
    # # 
    # 
    # 
    # output$bar_map <- renderLeaflet({
    #   reactive_mh <- reactive_mh_data()
    #   
    #   leaflet(reactive_mh) %>% 
    #     addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
    #     setView(lng = 78.9629, lat = 20.5937, zoom = 5) %>%
    #     
    #     addMinicharts(reactive_mh$longitude, reactive_mh$latitude,chartdata = reactive_mh[, 4:8], colorPalette = colors_for_mh, width = 60,height=120, transitionTime = 0)
    # 
    # })
    # 
    
    colors_for_mh <- c("#06C", "#8481DD", "#4CB140","#005F60","#F4C145")
    
    reactive_mh_data <- reactive({
      new_mhdata |>   
        group_by(Country_Code, !!sym(input$`choice_type`), latitude, longitude) |> 
        summarize(count = n()) |> 
        pivot_wider(names_from = !!sym(input$`choice_type`), values_from = count, values_fill = 0)
    })
    
    output$bar_map <- renderLeaflet({
      reactive_mh <- reactive_mh_data()
      
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
        setView(lng = 78.9629, lat = 20.5937, zoom = 5) %>%
        
        addMinicharts(reactive_mh$longitude, reactive_mh$latitude, chartdata = reactive_mh[, 4:8], 
                      colorPalette = colors_for_mh, width = 60, height = 120, transitionTime = 0)
    })
    
    
    
    mh_yn <- glp %>% group_by(Country, I13) %>% summarize(count = n()) %>% pivot_wider(names_from = I13, values_from = count)
    glp_indicators <- glp %>% rename(anxiety = 'C1_6', death = 'C1_4', loneliness = 'C1_1') %>%
      mutate(anxiety_death = ifelse(anxiety == "Yes" & death == "Yes", "Yes", "No"),
             anxiety_loneliness = ifelse(anxiety == "Yes" & loneliness == "Yes", "Yes", "No"),
             death_loneliness = ifelse(death == "Yes" & loneliness == "Yes", "Yes", "No"),
             all_three = ifelse(anxiety == "Yes" & death == "Yes" & loneliness == "Yes", "Yes", "No")) %>%
      select(Country, anxiety, death, loneliness, anxiety_death, anxiety_loneliness, death_loneliness, all_three)
    
    glp_summary <- glp_indicators %>% group_by(Country) %>% summarise(across(anxiety:all_three, ~ sum(. == "Yes"), .names = "count_{col}"))
    
   # countries <- jsonlite::fromJSON(txt ="F:/PE & RE Electives Semester-3/MentalHealthViz/Shapefiles/World_Countries_Generalized.json")
    
    output$plot_yn <- renderPlotly({
      param_mhyn <- input$parameter_mhyn
      plot_ly(data = mh_yn, type = "choropleth", locations = ~Country, z = ~get(param_mhyn), colorscale = "Blues", locationmode = "country names") %>%
        colorbar(title = "No of answers") %>%
        layout(geo = list(scope = "world"), title = paste("Mental Health Indicators -", param_mhyn))
    })
    
    selected_column <- reactive({
      if (all(c("Anxiety", "Death", "Loneliness") %in% input$conditions)) {
        "count_all_three"
      } else if (all(c("Anxiety", "Death") %in% input$conditions)) {
        "count_anxiety_death"
      } else if (all(c("Anxiety", "Loneliness") %in% input$conditions)) {
        "count_anxiety_loneliness"
      } else if (all(c("Death", "Loneliness") %in% input$conditions)) {
        "count_death_loneliness"
      } else if ("Anxiety" %in% input$conditions) {
        "count_anxiety"
      } else if ("Death" %in% input$conditions) {
        "count_death"
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
      glp %>% rename(anxiety = 'C1_6', death = 'C1_4', loneliness = 'C1_1')%>%  
        filter(Country == input$`pyramid-dropdown`) %>%  
        filter(Gender %in% c("Female", "Male")) %>%   
        group_by(Gender, Age_Group) %>%
        summarise(across(anxiety:loneliness, ~ sum(. == "Yes"), .names = "count_{col}")) |> 
        pivot_wider(names_from = Gender, values_from = c(count_anxiety,count_death, count_loneliness))  %>%
        mutate(count_anxiety_Male  = -count_anxiety_Male,count_death_Male=-count_death_Male,count_loneliness_Male=-count_loneliness_Male)  # Make male counts negative
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
                          choices = c("Anxiety", "Death among people I know", "Loneliness"), 
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
            condition = "input['condition-dropdown'] == 'Death among people I know' && input['value-type-dropdown'] == 'Absolute Values'",
            div(plotlyOutput("death_plot"),align="center")
          ),
          conditionalPanel(
            condition = "input['condition-dropdown'] == 'Death among people I know' && input['value-type-dropdown'] == 'Percentage'",
            div(plotlyOutput("death_per_plot"),align="center")
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
    
    
    #for death
    
    output$death_plot <- renderPlotly({
      
      pyramid_df_death <- filtered_data_mental_health()
      
      selected_country <- input$`pyramid-dropdown`
      
      plot_ly(pyramid_df_death) %>%
        add_trace(
          x = ~count_death_Male, y = ~Age_Group, 
          type = 'bar', name = 'Male',
          marker = list(color = '#303f70'),
          orientation = 'h'
        ) %>%
        add_trace(
          x = ~count_death_Female, y = ~Age_Group, 
          type = 'bar', name = 'Female',
          marker = list(color = '#bd5cb0'),
          orientation = 'h'
        ) %>%
        layout(
          title = paste("Population Pyramid-Death for", selected_country),
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
          xaxis = list(title= 'Percengtage(%) of participants', tickmode = 'array', tickvals = c(-10, -8, -6,-4,-2, 0, 2, 4, 6,8,10),
                       ticktext = c('10', '8', '6','4','2', '0', '2', '4', '6','8','10')),
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
          title = paste("Population Pyramid-Loneliness for", selected_country),
          yaxis = list(title = "Age Group"),
          xaxis = list(title= 'Percengtage(%) of participants', tickmode = 'array', tickvals = c(-10, -8, -6,-4,-2, 0, 2, 4, 6,8,10),
                       ticktext = c('10', '8', '6','4','2', '0', '2', '4', '6','8','10')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    }) 
    
    
    
    #for death
    output$death_per_plot <- renderPlotly({
      
      pyramid_df_death <- filtered_data_mental_health()
            total_count <- glp_total()

      selected_country <- input$`pyramid-dropdown`
      
      plot_ly() %>%
        add_trace(
          x = (pyramid_df_death$count_death_Male/total_count$count)*100, y = pyramid_df_death$Age_Group, 
          type = 'bar', name = 'Male',
          marker = list(color = '#303f70'),
          orientation = 'h'
        ) %>%
        add_trace(
          x = (pyramid_df_death$count_death_Female/total_count$count)*100, y = pyramid_df_death$Age_Group, 
          type = 'bar', name = 'Female',
          marker = list(color = '#bd5cb0'),
          orientation = 'h'
        ) %>%
        layout(
          title = paste("Population Pyramid-Death for", selected_country),
          yaxis = list(title = "Age Group"),
          xaxis = list(title= 'Percengtage(%) of participants', tickmode = 'array', tickvals = c(-10, -8, -6,-4,-2, 0, 2, 4, 6,8,10),
                       ticktext = c('10', '8', '6','4','2', '0', '2', '4', '6','8','10')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    }) 
    
  })































