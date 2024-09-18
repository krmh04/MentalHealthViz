library(dplyr)
library(readxl)
library(tidyverse)
library(tidygeocoder) 
library(pyramid)
library(plotly)
library(RColorBrewer)
library(likert)
library(HH)
library(patchwork)
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

names(new_mhdata) <- gsub("[\r\n]", "", names(new_mhdata))

new_mhdata<-  new_mhdata |>  pivot_wider(names_from = Anxious, values_from = count, values_fill = 0)


new_mhdata_for_likert <- new_mhdata |>   
  group_by(Country_Code, Anxious) |> 
  summarize(count = n())

new_mhdata_for_likert <- new_mhdata_for_likert %>%
  filter(!is.na(Anxious))

new_mhdata_for_likert_wide<-  new_mhdata_for_likert |>  pivot_wider(names_from = Anxious, values_from = count, values_fill = 0)

new_mhdata_for_likert_wide <- new_mhdata_for_likert_wide %>%
  mutate(Country_Code = as.factor(Country_Code))



  
HH::likert(Country_Code ~ ., new_mhdata_for_likert_wide, main="Divergent bar chart for people with Anxiety")

new_mhdata <- read_csv("F:/PE & RE Electives Semester-3/MentalHealthViz/geocoded_ProfOgunbode.csv")
names(new_mhdata) <- gsub("[\r\n]", "", names(new_mhdata))

 

 

dat_longer <-new_mhdata |>   
  group_by(Country_Code, Anxious) |> 
  summarize(count = n())

dat_longer<-  dat_longer |>  pivot_wider(names_from = Anxious, values_from = count, values_fill = 0)


dat_longer <- dat_longer %>% rename(None = 'NA')
dat_for_plot <- dat_longer %>%
  pivot_longer(
    cols = 2:7,  
    names_to = "Anxious",      
    values_to = "count"        
  )
 

dat_filtered <- dat_for_plot %>%
  filter(Anxious != "None")

 
dat_filtered$Anxious <- trimws(as.character(dat_filtered$Anxious))

  
colnames(dat_longer) <- gsub("[\r\n]", "", colnames(dat_longer))


# Define the levels in the order you want them
levels_order <- c("Not at all", "Somewhat", "Moderately", "Very much", "Extremely")


dat_filtered <- dat_filtered %>%
  arrange(Country_Code,Anxious)


# Convert to ordered factor
dat_filtered$Anxious <- factor(dat_filtered$Anxious, 
                             levels = levels_order,
                             ordered = TRUE)

computed_values <- dat_filtered |> 
  mutate(
    middle_shift = sum(count[1:2]),
    lagged_count = lag(count, default = 0),
    left = cumsum(lagged_count) - middle_shift,
    right = cumsum(count) - middle_shift,
    middle_point = (left + right) / 2,
    width = right - left,
   )


bar_width <- 0.75
computed_values |> 
  ggplot() +
  geom_tile(
    aes(
      x = middle_point, 
      y = Country_Code,
      width = width,
      fill = Anxious
    ),
    height = bar_width
  )
breaks = seq(-1200, 1200, by = 200)
labels = as.character(abs(breaks))


factor_dat <- computed_values |> 
  mutate(
    label = factor(
      Country_Code,
      levels = dat_longer$Country_Code
    ) |> fct_rev()
  ) 

initial_diverging_bar_plot <- factor_dat |> 
  ggplot() +
  geom_tile(
    aes(
      x = middle_point, 
      y = Country_Code,
      width = width,
      fill = Anxious
    ),
    height = bar_width
  )+scale_x_continuous(
    breaks = breaks,  # Define your custom tick positions
    labels =labels)
initial_diverging_bar_plot +geom_vline(
  xintercept = 0,
  color = 'black',
  linewidth = 0.25
)+theme(
  legend.position = "bottom",
    panel.grid = element_blank(),
  panel.grid.major = element_blank(),  
  panel.grid.minor = element_blank()
)+labs(x="No of responses",y="Country")




grey_color = '#bdbfc1'

dat_for_none <- dat_for_plot %>%
  filter(Anxious == "None")


neither_chart <- dat_for_none |>
  ggplot() +
  geom_col(
    aes(
      y =Country_Code,
      x =count ,
    ),
    fill = grey_color,
    width = bar_width
  )+  geom_text(
    aes(
      y = Country_Code,
      x = count,
      label = count
    ),
    size = 2.5,
    color = 'black',
    hjust = -0.1
  )+labs(title="None")+scale_x_continuous(
    limits = c(0, 8))+theme(
      legend.position = 'none',
      strip.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )

    
 combined_plot<-initial_diverging_bar_plot +
   neither_chart +
   plot_layout(
     ncol = 3,
     widths = c(3,1)
   )
 combined_plot
 
 
 
 

 dat_longer <-new_mhdata |>   
   group_by(Country_Code, Anxious) |> 
   summarize(count = n())
 
 dat_longer<-  dat_longer |>  pivot_wider(names_from = Anxious, values_from = count, values_fill = 0)
  
 
 dat_longer <- dat_longer %>% rename(None = 'NA')
 dat_for_plot <- dat_longer %>%
   pivot_longer(
     cols = 2:7,  
     names_to = "Anxious",      
     values_to = "count"        
   )
 
 colnames(dat_longer) <- gsub("[\r\n]", "", colnames(dat_longer))
 
 dat_filtered <- dat_for_plot %>%
   filter(Anxious != "None")
 
 
 dat_filtered$Anxious <- trimws(as.character(dat_filtered$Anxious))
 
 
 
 
 
 
 levels_order <- c("Not at all", "Somewhat", "Moderately", "Very much", "Extremely")
 dat_filtered$Anxious <- factor(dat_filtered$Anxious, levels = levels_order, ordered = TRUE)
 
 # Assuming dat_filtered is your original dataset
 dat_filtered <- dat_filtered %>%
   arrange(Country_Code, Anxious) %>%
   group_by(Country_Code) %>%
   mutate(
     total_responses = sum(count),
     percentage = count / total_responses * 100
   ) %>%
   ungroup()
 
 # Convert to ordered factor
 
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
 
 bar_width <- 0.75
 
 # Create the plot
 diverging_bar_plot_percentages <- computed_values %>%
   mutate(
     label = factor(Country_Code, levels = unique(Country_Code)) %>% fct_rev()
   ) %>%
   ggplot() +
   geom_tile(
     aes(
       x = middle_point,
       y = Country_Code,
       width = width,
       fill = Anxious
     ),
     height = bar_width
   ) +
   scale_x_continuous(
     breaks = seq(-100, 100, by = 20),
     labels = function(x) paste0(abs(x), "%")
   ) +
   geom_vline(
     xintercept = 0,
     color = 'black',
     linewidth = 0.25
   ) +
   theme(
     legend.position = "bottom",
     panel.grid = element_blank(),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank()
   ) +
   labs(x = "Percentage of responses", y = "Country")
 
 # Display the plot
 print(diverging_bar_plot_percentages)
 
 
 

 
 
 
 
 
 
 
 
 
 
 
