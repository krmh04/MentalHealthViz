library(shiny)
library(bs4Dash)
library(leaflet)
library(dplyr)
library(readxl)
library(sf)
library(tidyverse)
library(tidygeocoder)
library(ggplot2)
library(plotly)
library(leaflet.minicharts)
library(ggiraph)
library(DT)
options(shiny.maxRequestSize = 500 * 1024^2)
# Visualization code
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
        menuItem("Upload", tabName = "upload", icon = icon("upload")),
        
        menuItem("Population Samples", tabName = "samples", icon = icon("person")),
        menuItem("Mental Health Indicators", tabName = "mentalhealth", icon = icon("notes-medical")),
        menuItem("Ordinal data", tabName = "ordinal", icon = icon("chart-column")),
        menuItem("Divergent bar chart", tabName = "stacked", icon = icon("boxes-stacked")),
        
        menuItem("Population Pyramid", tabName = "pyramid", icon = icon("cubes"))
 
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
      #shiny-notification-panel {
  top: 81px;
  bottom: unset;
  left: 909px;
  right: 0;
  font-weight:bold;
  margin-left: auto;
  margin-right: auto;
  width: 100%;
  max-width: 450px;
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
          tabItem(tabName = "upload",
                  tabBox(
                    width = 12,
                    tabPanel("Upload CSV",
                             fluidRow(
                               column(4,
                                      fileInput("filedata", "Choose CSV File",
                                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                               ),
                               column(4,
                                      actionButton("remove_csv", "Remove Current CSV", 
                                                   class = "btn-danger", 
                                                   style = "margin-top: 25px;")
                               )
                             ),
                             fluidRow(
                               column(12,
                                      h4("Rename Columns"),
                                      DTOutput("rename_table"),
                                      actionButton("apply_names", "Apply New Column Names")
                               )
                             )
                            
                    ),
                    tabPanel("View Data",
                             fluidRow(
                               column(12,
                                      h4("View Data"),
                                      DTOutput("view_table")
                               )
                             )
                    )
                  )
          ),
         
        tabItem(
          tabName = "samples",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              uiOutput("refinement_ui"),
              checkboxInput("check_viz", "Is the Visualization Incorrect? ", FALSE),
              conditionalPanel(
                condition = "input.check_viz == true",
                verbatimTextOutput("viz_check_output"),
                selectInput("country_col", "Country Column", choices = NULL),
                selectInput("gender_col", "Gender Column", choices = NULL),
                selectInput("lat_col", "Latitude Column", choices = NULL),
                selectInput("lon_col", "Longitude Column", choices = NULL),
                uiOutput("mismatch_ui"),
                actionButton("update_viz", "Update Visualization")
              )
            ),
            mainPanel(
              width = 9,
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
          tabName = "stacked",
          # Dropdown to select the type of population pyramid
          h4("Respondents' Feelings with regards to Climate Anxiety"),
          
          fluidRow(
            column(width = 6,
                   # Dropdown to select the type of population pyramid
                   selectInput("choice_type_stacked", "Select the parameter", 
                               choices = names(new_mhdata)[4:10]) 
            ),
            column(width=6,
                   div(style = "height:416px;width:743px;margin-left: -282px", 
                   girafeOutput("diverging_bar_plot_percentages")
                   )
                   
           )
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

    countries <- read_sf('F:/PE & RE Electives Semester-3/MentalHealthViz/Shapefiles/copy_4.shp',options = "ENCODING=WINDOWS-1252")
    countries <- countries %>% rename(Country = NAME_LONG)
  
    uploaded_data <- reactiveVal()
    renamed_data <- reactiveVal()
    
    # Reactive values for column selections
    selected_cols <- reactiveValues(
      country = NULL,
      gender = NULL,
      lat = NULL,
      lon = NULL
    )
    
    # Read the uploaded CSV file
    observeEvent(input$filedata, {
      req(input$filedata)
      data <- read.csv(input$filedata$datapath)
      uploaded_data(data)
      renamed_data(data)  
      
      updateColumnSelections(data)
    })
    
    # Remove CSV and reset app state
    observeEvent(input$remove_csv, {
      uploaded_data(NULL)
      renamed_data(NULL)
      selected_cols$country <- NULL
      selected_cols$gender <- NULL
      selected_cols$lat <- NULL
      selected_cols$lon <- NULL
      
        
      # Reset column selection inputs
      updateSelectInput(session, "country_col", choices = NULL)
      updateSelectInput(session, "gender_col", choices = NULL)
      updateSelectInput(session, "lat_col", choices = NULL)
      updateSelectInput(session, "lon_col", choices = NULL)
      
      # Show a notification to the user
      showNotification("CSV file removed and app state reset.",duration=12, type = "warning")
    })
    
    # Helper function to update column selections
    updateColumnSelections <- function(data) {
      selected_cols$country <- names(data)[grep("country", tolower(names(data)))]
      selected_cols$gender <- names(data)[grep("gender", tolower(names(data)))]
      selected_cols$lat <- names(data)[grep("lat", tolower(names(data)))]
      selected_cols$lon <- names(data)[grep("lon", tolower(names(data)))]
    
      updateSelectInput(session, "country_col", choices = names(data), selected = selected_cols$country)
      updateSelectInput(session, "gender_col", choices = names(data), selected = selected_cols$gender)
      updateSelectInput(session, "lat_col", choices = names(data), selected = selected_cols$lat)
      updateSelectInput(session, "lon_col", choices = names(data), selected = selected_cols$lon)
      }
    
    # Render the rename table
    output$rename_table <- renderDT({
      req(uploaded_data())
      df <- data.frame(
        Original = names(uploaded_data()),
        New = names(uploaded_data()),
        stringsAsFactors = FALSE
      )
      datatable(df, editable = TRUE)
    })
    
    # Apply new column names
    observeEvent(input$apply_names, {
      req(uploaded_data())
      new_names <- input$rename_table_cell_edit
      if (!is.null(new_names) && nrow(new_names) > 0) {
        current_data <- uploaded_data()
        for (i in seq_len(nrow(new_names))) {
          if (new_names$col[i] == 2) {  # Only change names in the "New" column
            old_name <- names(current_data)[new_names$row[i]]
            new_name <- new_names$value[i]
            if (old_name != new_name) {
              names(current_data)[names(current_data) == old_name] <- new_name
            }
          }
        }
        renamed_data(current_data)
        showNotification("Columns successfully renamed!",duration=12, type = "message")
        
        updateColumnSelections(current_data)
      }
    })
    
    # View renamed data
    output$view_table <- renderDT({
      req(renamed_data())
      data_to_show <- renamed_data()
      datatable(data_to_show,
                options = list(
                  scrollX = TRUE,
                  scrollCollapse = TRUE,
                  autoWidth = TRUE
                ))
    })
    mismatches <- reactive({
      req(renamed_data(), countries)
      data_c <- renamed_data() %>%
        group_by(Country) %>%
        summarize(count = n())
      
      country_names <- unique(data_c$Country)
      shape_country_names <- unique(countries$Country)
      
      # Find mismatches
      unmatched <- setdiff(country_names, shape_country_names)
      return(unmatched)
    })
    output$mismatch_ui <- renderUI({
      unmatched <- mismatches()
      if (length(unmatched) > 0) {
        lapply(1:length(unmatched), function(i) {
          selectInput(
            paste0("recode_", unmatched[i]),
            label = paste("Recode", unmatched[i]),
            choices = unique(countries$Country),
            selected = NULL
          )
        })
      } else {
        return(NULL)
      }
    })
   observeEvent(input$update_viz, {
  selected_cols$country <- input$country_col
  selected_cols$gender <- input$gender_col
  selected_cols$lat <- input$lat_col
  selected_cols$lon <- input$lon_col
  
  req(mismatches())
  unmatched <- mismatches()
  
  recode_map <- lapply(unmatched, function(name) {
    input[[paste0("recode_", name)]]
  })
  
  recode_map <- setNames(recode_map, unmatched)
  
  # Recode the country names in `data_c`
  recoded_data <- renamed_data() %>%
    mutate(Country = recode(Country, !!!recode_map))
  
  renamed_data(recoded_data)  # Store the recoded data
})
    
    # Reactive expression for the map data
    map_data <- reactive({
      req(renamed_data(), selected_cols$country, selected_cols$gender, selected_cols$lat, selected_cols$lon)
      data <- renamed_data()
      
      data %>%
        select(Country = selected_cols$country, 
               Gender = selected_cols$gender, 
               latitude = selected_cols$lat, 
               longitude = selected_cols$lon) %>%
        group_by(Country, Gender, latitude, longitude) %>%
        summarize(count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = Gender, values_from = count, values_fill = 0)
    })
    
   
    
    output$map <- renderLeaflet({
      req(map_data())
      req(renamed_data())
      data_c <- renamed_data()
      gender_data <- map_data()
     
     
      data_c <- data_c %>%
         group_by(Country) %>%
         summarize(count = n())
       countries <- merge(countries, data_c, by = 'Country', all.x = F)
      
      
       color_mapping <- c(
         "Male" = "#fe9929", 
         "Female" = "#dd22dd", 
         "Other" = "#c8F213", 
         "Don't know/ Prefer not to say" = "#171717"
       )
       actual_categories <- setdiff(names(gender_data), c("Country", "latitude", "longitude"))
       colors <- color_mapping[actual_categories]
       
       # Ensure that missing categories in the color mapping are handled (if new/unexpected categories are present)
        
       
      costBins <- c(500, 700,900,1100,Inf)
      # custom_colors <- c("#fde8cd", "#fddcc0", "#fcc5aa", "#d57b7b")
      custom_colors <- c("#f0f9e8", "#bae4bc", "#7bccc4", "#2b8cbe")
        paletteBinned <- colorBin(palette = custom_colors, domain = countries$count, bins = costBins)
      
      leaflet(gender_data) %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
        setView(lng = mean(gender_data$longitude), lat = mean(gender_data$latitude), zoom = 2) %>%
        
        addPolygons(data = countries,
                    # soften the weight of the state borders
                    weight = 1,

                    # values >1 simplify the polygons' lines for less detail but faster loading
                    smoothFactor = .3,

                    # set opacity of polygons
                    fillOpacity = .75,

                    # specify that the each state should be colored per paletteNum()
                    fillColor = ~paletteBinned(countries$count)) %>% addLegend(pal = paletteBinned, values = countries$count,
                                                                             title = '<small>No of participants</small>',
                                                                             labels = c("500-700", "700-900", "900-1100", ">1100"),
                                                                             labFormat = function(type, cuts, p) {
                                                                                labels <- c("500-700", "700-900", "900-1100", ">1100")
                                                                               return(labels)
                                                                             },
                                                                             position = 'bottomleft') |> 
        
        addMinicharts(
          gender_data$longitude, 
          gender_data$latitude, 
          type = "pie",
          chartdata = gender_data[, actual_categories],
          colorPalette = unname(colors),
          width = (gender_data$Male +gender_data$Female)/65,
          transitionTime = 0,
          popupOptions = list(closeButton = TRUE)
                  )
    })
    
    # For ordinal data
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
        
        addMinicharts(reactive_mh$longitude, reactive_mh$latitude,type = "pie", chartdata = reactive_mh[, 4:8], 
                      colorPalette = colors_for_mh, width = 35, height = 35, transitionTime = 0)
    })
   
    
    
    #For divergent stacked bar charts
    
     processed_data <- reactive({
      choice_type_stacked <- input$choice_type_stacked
      
      dat_longer <- new_mhdata |>   
        group_by(Country_Code, !!sym(choice_type_stacked)) |> 
        summarize(count = n())
      
      dat_longer <- dat_longer |>  
        pivot_wider(names_from = !!sym(choice_type_stacked), values_from = count, values_fill = 0)
      
      dat_longer <- dat_longer %>% rename(None = 'NA')
      dat_for_plot <- dat_longer %>%
        pivot_longer(
          cols = 2:7,  
          names_to = choice_type_stacked,      
          values_to = "count") 
      
      colnames(dat_longer) <- gsub("[\r\n]", "", colnames(dat_longer))
      
      dat_filtered <- dat_for_plot %>%
        filter(!!sym(choice_type_stacked) != "None")
      
      dat_filtered[[choice_type_stacked]] <- trimws(as.character(dat_filtered[[choice_type_stacked]]))
      
      levels_order <- c("Not at all", "Somewhat", "Moderately", "Very much", "Extremely")
      
      dat_filtered[[choice_type_stacked]] <- factor(dat_filtered[[choice_type_stacked]], 
                                                    levels = levels_order, ordered = TRUE)
       dat_filtered <- dat_filtered %>%
        arrange(Country_Code, !!sym(choice_type_stacked)) %>%
        group_by(Country_Code) %>%
        mutate(
          total_responses = sum(count),
          percentage = count / total_responses * 100
        ) %>%
        ungroup()
   
      
      computed_values <- dat_filtered %>%
        group_by(Country_Code) %>%
        mutate(
          middle_shift = sum(percentage[1:2]),
          lagged_percentage = lag(percentage, default = 0),
          left = cumsum(lagged_percentage) - middle_shift,
          right = cumsum(percentage) - middle_shift,
          middle_point = (left + right) / 2,
          width = right - left
        ) %>%
        ungroup()
      
      return(computed_values)
    })
    
    # Render the plot
    output$diverging_bar_plot_percentages <- renderGirafe({
      bar_width <- 0.75
    
      # Create the plot
      diverging_bar_plot_percentages <- processed_data() %>%
        mutate(
          label = factor(Country_Code, levels = unique(Country_Code)) %>% fct_rev()
        ) %>%
        ggplot() +
        geom_tile_interactive(
          aes(
            x = middle_point,
            y = Country_Code,
            width = width,
            fill = !!sym(input$choice_type_stacked),tooltip = paste0("Country: ", Country_Code, "<br>",
                                                                     "Percentage: ",  round(abs(width), 2),"%")
          ),
          height = bar_width
        ) +scale_fill_manual(values = rev(c("#440154", "#3B528B", "#21908C","#5DC863","#FDE725")))+
        scale_x_continuous(
          breaks = seq(-100, 100, by = 20),
          labels = function(x) paste0(abs(x), "%")
        ) +
        theme(
          legend.position = "right",
          panel.grid = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        ) +labs(x = "Percentage of responses", y = "Country")
      
      # return(diverging_bar_plot_percentages)
      return(girafe(ggobj = diverging_bar_plot_percentages))
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
                       ticktext = c('10%', '8%', '6%','4%','2%', '0', '2%', '4%', '6%','8%','10%')),
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
                       ticktext = c('10%', '8%', '6%','4%','2%', '0', '2%', '4%', '6%','8%','10%')),
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
                       ticktext = c('10%', '8%', '6%','4%','2%', '0', '2%', '4%', '6%','8%','10%')),
          barmode = 'overlay',
          bargap = 0.1,
          autosize = F,
          margin = list(l = 100, r = 20, t = 70, b = 70)
        ) %>%
        config(displayModeBar = FALSE)
    }) 
    
  })































