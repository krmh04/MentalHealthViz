library(dplyr)
library(readxl)
library(tidyverse)
library(tidygeocoder) 
library(pyramid)
library(plotly)
library(RColorBrewer)

glp <- read.csv("F:/PE & RE Electives Semester-3/MentalHealthViz/Main_GLP.csv")
geocoded_file_path <- "F:/PE & RE Electives Semester-3/MentalHealthViz/geocoded_glp.csv"
glp_geocoded_data <- read.csv(geocoded_file_path)

glp_total <- glp %>% filter(Country=="India") %>%
  summarize(count = n())
 
glp <- glp %>%
  rename(anxiety = 'C1_6', depression = 'C1_4', loneliness = 'C1_1')

pyramid_df$`Age_Group` <- factor(pyramid_df$`Age_Group`)

pyramid_df <- glp %>% 
  group_by(Country,Gender) %>%
  summarize(count = n())


pyramid_df <- pyramid_df %>% pivot_wider(names_from = `Gender`, values_from =count) |> mutate(sumindex = sum(c_across(c(1:4)), na.rm = T))

pyramid_df_female <- pyramid_df %>% filter(Gender == "Female")
pyramid_df_male <- pyramid_df %>% filter(Gender == "Male")

# Create the plot
filtered_data_mental_health<-
  glp %>% rename(anxiety = 'C1_6', depression = 'C1_4', loneliness = 'C1_1')%>%  
    filter(Country == "India") %>%  
    filter(Gender %in% c("Female", "Male")) %>%   
    group_by(Gender, Age_Group) %>%
    summarise(across(anxiety:loneliness, ~ sum(. == "Yes"), .names = "count_{col}")) |> 
    pivot_wider(names_from = Gender, values_from = c(count_anxiety,count_depression, count_loneliness))  %>%
    mutate(count_anxiety_Male  = -count_anxiety_Male,count_depression_Male=-count_depression_Male,count_loneliness_Male=-count_loneliness_Male)  # Make male counts negative

plot_ly() %>%
  add_trace(
    x = (filtered_data_mental_health$count_anxiety_Male/glp_total$count)*100, y = filtered_data_mental_health$Age_Group, 
    type = 'bar', name = 'Male',
    marker = list(color = '#303f70'),
    orientation = 'h'
  ) %>%
  add_trace(
    x = (filtered_data_mental_health$count_anxiety_Female/glp_total$count)*100, y = filtered_data_mental_health$Age_Group, 
    type = 'bar', name = 'Female',
    marker = list(color = '#bd5cb0'),
    orientation = 'h'
  ) %>%
  layout(
    title = paste("Population Pyramid-Anxiety for"),
    yaxis = list(title = "Age Group"),
    xaxis = list(title= 'Count of participants', tickmode = 'array', tickvals = c(-100, -80, -60,-40,-20, 0, 20, 40, 60,80,100),
                 ticktext = c('100', '80', '60','40','20', '0', '20', '40', '60','80','100')),
    barmode = 'overlay',
    bargap = 0.1,
    autosize = F,
    margin = list(l = 100, r = 20, t = 70, b = 70)
  ) %>%
  config(displayModeBar = FALSE)
 

 
gender_df<- new_df%>% pivot_wider(names_from = Gender, values_from =count)
 

max_count <- max(pyramid_df$count)


glp <- read.csv("F:/PE & RE Electives Semester-3/nfhs5Viz/Main_GLP.csv",encoding="UTF-8")
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



countries <- read_sf('F:/PE & RE Electives Semester-3/nfhs5Viz/Shapefiles/copy_4.shp',options = "ENCODING=WINDOWS-1252")

 
countries <- countries %>% rename(Country = NAME_LONG)


is.element(glp_total$Country, countries$Country) %>%
  all()

countries <- merge(countries, glp_total, by = 'Country', all.x = F)

 
paletteNum <- colorNumeric('Blues', domain = countries$count)

costBins <- c(500, 600,800, 1000,1100, 1200, Inf)
paletteBinned <- colorBin('OrRd', domain = countries$count, bins = costBins)

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels)  %>% 
  setView(lng = 78.9629, lat = 20.5937, zoom = 4) %>% 
  addPolygons(data = countries,
              # soften the weight of the state borders
              weight = 1,
              
              # values >1 simplify the polygons' lines for less detail but faster loading
              smoothFactor = .3,
              
              # set opacity of polygons
              fillOpacity = .75,
              
              # specify that the each state should be colored per paletteNum()
              fillColor = ~paletteBinned(countries$count))

glp_summary <- glp %>% group_by(Country,Gender,Age_Group) %>% summarise(across(anxiety:depression, ~ sum(. == "Yes"), .names = "count_{col}"))
filtered_data <- 
  glp %>% 
    filter(Country == "Argentina") %>%  
    filter(Gender %in% c("Female", "Male")) %>%   
    group_by(Gender, Age_Group) %>%
    summarise(across(anxiety:loneliness, ~ sum(. == "Yes"), .names = "count_{col}")) |> 
  pivot_wider(names_from = Gender, values_from = c(count_anxiety,count_depression, count_loneliness))  




new_mhdata <- read_csv("F:/PE & RE Electives Semester-3/MentalHealthViz/geocoded_ProfOgunbode.csv")
 

   # new_mhdata <- new_mhdata %>% geocode(Country_Code, method = 'osm', lat = latitude, long = longitude)


new_mhdata <- new_mhdata |>   
  group_by(Country_Code, Anxious,latitude,longitude) |> 
  summarize(count = n())


new_mhdata<-  new_mhdata |>  pivot_wider(names_from = Anxious, values_from = count, values_fill = 0)



 

 names(new_mhdata) <- gsub("[\r\n]", "", names(new_mhdata))





  

colors <- c("#3093e5", "#fcba50", "#a0d9e8","#dfdfdf","#000000")
leaflet(new_mhdata) %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  setView(lng = 78.9629, lat = 20.5937, zoom = 5) %>%
  
  addMinicharts(new_mhdata$longitude, new_mhdata$latitude,chartdata = new_mhdata[, 4:8], colorPalette = colors, width = 60,height=120, transitionTime = 0)




very_new_data<-read_xlsx("F:/PE & RE Electives Semester-3/MentalHealthViz/geocoded_new_mhdata.xlsx")



output$bar_map <- renderLeaflet({
  leaflet(new_mhdata) %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
    setView(lng = 78.9629, lat = 20.5937, zoom = 5) %>%
    
    addMinicharts(new_mhdata$longitude, new_mhdata$latitude,chartdata = new_mhdata[, 4:8], colorPalette = colors, width = 60,height=120, transitionTime = 0)
  
  
  
 })
 







glp_summary <- glp_indicators %>% group_by(Country) %>% summarise(across(anxiety:all_three, ~ sum(. == "Yes"), .names = "count_{col}"))


print(filtered_countries)













 
