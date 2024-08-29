library(dplyr)
library(readxl)
library(tidyverse)
library(tidygeocoder) 
library(pyramid)
library(plotly)
library(RColorBrewer)

glp <- read.csv("F:/PE & RE Electives Semester-3/nfhs5Viz/Main_GLP.csv")

glp_total <- glp %>% 
group_by(Country) %>%
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


 
