library(tidyverse)
library(plotly)
library(leaflet)
library(dplyr)
library(ggplot2)
library(leaflet.extras)  # Load leaflet.extras

# Read a CSV file
data <- read.csv("/Users/vivekkumar/Desktop/finalproject_ait/updated_foodbankk.csv")

# View the first few rows
head(data)

# Checking data
colnames(data)
summary(data)

#cleaning:
# Remove the first row
household_summary2 <- household_summary[-1, ]
# Check the updated dataset
head(household_summary2)


#visualizations:
#maps:
# Create a basic interactive map
maps<- leaflet(data) %>%
  addTiles() %>%  # Add default map tiles
  addCircleMarkers(
    lng = ~data$Longitude,  # Replace with your longitude column name
    lat = ~data$Latitude,   # Replace with your latitude column name
    popup = ~data$Locality,# Replace with a relevant column for popups (e.g., Name, Address)
    color = "black",      # Corrected argument name for stroke color
    fillColor = "cornsilk",
    fillOpacity = 0.5
  )%>%
  setView(lng = mean(data$Longitude), lat = mean(data$Latitude), zoom = 7)

maps

#locations 
year_householdsserved_summary <- data %>%
  group_by(HouseholdsServed, Year, Locality) %>%  # Replace with the actual column names
  summarize(Count = n()) %>%    # Count the occurrences
  ungroup()
plot_ly(
  data = household_summary2,
  x = ~ Locality,  # Replace with actual column name
  y = ~Totalhouseholdsserved,     # Replace with actual column name
  color = ~as.factor(Year),  # Color based on Year
  type = 'bar',
  text = ~paste("Year:", Year,"<br>Location:", Locality, "<br>HouseholdsServed:", Totalhouseholdsserved),  # Add tooltips
  hoverinfo = 'text'
) %>%
  layout(
    title = "Comparison of House Holds served among localities Across Years",
    xaxis = list(title = "Location", tickangle = 45),
    yaxis = list(title = "Count"),
    barmode = 'group' , # 'group' for side-by-side bars, 'stack' for stacked bars
    colorway = c("#636EFA", "#EF553B", "#00CC96", "#AB63FA", "#FFA15A")  # Custom color set
    
  )
#food
food_distribution_summary <- data %>%
  group_by(Locality, Year) %>%
  summarize(TotalFoodDistributed = sum(PoundsFoodDistributed, na.rm = TRUE))
plot_ly(
  data = food_distribution_summary,
  x = ~Locality,  # Replace with actual column name
  y = ~TotalFoodDistributed,     # Replace with actual column name
  color = ~as.factor(Year),  # Color based on Year
  type = 'bar',
  text = ~paste("Year:", Year, "<br>Location:", Locality),  # Add tooltips
  hoverinfo = 'text'
) %>%
  layout(
    title = "Comparison of Locations Across Years",
    xaxis = list(title = "Location", tickangle = 45),
    yaxis = list(title = "Count"),
    barmode = 'group'  # 'group' for side-by-side bars, 'stack' for stacked bars
  )

#research question:
#How does food distribution vary across locality over change in time ?

# We are finding food summary. In this we are doing a subset from whole dataset we take locality, year, and 
#sum of food distributed in that year. and using plotly.

# to get the correctky food distribution we can use both house holds served and individuals served
food_summary <- data %>%
  group_by(Locality, Year) %>%  
  summarize(TotalPoundsDistributed = sum(PoundsFoodDistributed, na.rm = TRUE))  # Replace FoodDistributed with your column name

# View the result
print(food_summary)
write.csv(food_summary, "Food_Distribution_Summary.csv", row.names = FALSE)

plot_ly(
  data = food_summary,
  x = ~Year,
  y = ~TotalPoundsDistributed,
  color = ~Locality,
  type = "bar",
  text = ~paste("Year:", Year, "<br>Locality:", Locality, "<br>Total Pounds Distributed:", TotalPoundsDistributed),
  hoverinfo = "text"
) %>%
  layout(
    title = "Food Distributed Across Localities Over Years",
    xaxis = list(title = "Year", tickangle = 45),  # Rotate x-axis labels
    yaxis = list(title = "Total Pounds of Food Distributed"),
    barmode = "group"  # Group bars side by side
  )

#household  
plot_ly(
  data = household_summary,
  x = ~Year,
  y = ~Totalhouseholdsserved,
  color = ~Locality,
  type = "bar",
  text = ~paste("Year:", Year, "<br>Locality:", Locality, "<br>Total house holds served:", Totalhouseholdsserved),
  hoverinfo = "text"
) %>%
  layout(
    title = "Households distr Across Localities Over Years",
    xaxis = list(title = "Year", tickangle = 45),  # Rotate x-axis labels
    yaxis = list(title = "Total Pounds of Food Distributed"),
    barmode = "group"  # Group bars side by side
  )

#year wise: 

yearly_summary <- food_summary %>%
  group_by(Year) %>%
  TotalPoundsDistributed = TotalPoundsDistributed
print(yearly_summary)

plot_ly(
  data = yearly_summary,
  x = ~Year,  # X-axis: Year
  y = ~TotalPoundsDistributed,  # Y-axis: Total Pounds Distributed
  type = "bar",  # Bar chart
  text = ~paste("Year:", Year, "<br>Total Distributed:", TotalPoundsDistributed),  # Hover text
  hoverinfo = "text",  # Display custom hover text
  marker = list(color = "skyblue", line = list(color = "darkblue", width = 1.5))  # Bar styling
) %>%
  layout(
    title = "Total Food Distributed Per Year",
    xaxis = list(title = "Year", tickangle = 0),  # Rotate x-axis labels if necessary
    yaxis = list(title = "Total Pounds of Food Distributed"),
    template = "plotly_white"  # Minimal, clean theme
  )

yearlyhousehold_summary <- data %>%
  group_by(Year) %>%
  summarize(Totalhouseholdserved = sum(HouseholdsServed))
print(yearlyhousehold_summary)

plot_ly(
  data = yearlyhousehold_summary,
  x = ~Year,  # X-axis: Year
  y = ~Totalhouseholdserved,  # Y-axis: Total Pounds Distributed
  type = "bar",  # Bar chart
  text = ~paste("Year:", Year, "<br>Total HouseHolds served:", Totalhouseholdserved),  # Hover text
  hoverinfo = "text",  # Display custom hover text
  marker = list(color = "skyblue", line = list(color = "darkblue", width = 1.5))  # Bar styling
) %>%
  layout(
    title = "Total households served Per Year",
    xaxis = list(title = "Year", tickangle = 0),  # Rotate x-axis labels if necessary
    yaxis = list(title = "Total Pounds of Food Distributed"),
    template = "plotly_white"  # Minimal, clean theme
  )


#end of research question 1

#research question 2
# How did the COVID-19 pandemic impact food distribution in Virginia? 

comparison <- food_summary %>%
  group_by(Locality) %>%
  summarize(
    ChangeInDistribution = TotalPoundsDistributed[Year == 2020]
  )
comparison <- comparison %>%
  mutate(
    Status = ifelse(ChangeInDistribution > 0, "Increase", "Decrease")
  )

plot_ly(
  data = comparison,
  x = ~Locality,
  y = ~ChangeInDistribution,
  color = ~Status,
  type = "bar",
  text = ~paste("Locality:", Locality, "<br>Change:", ChangeInDistribution, "<br>Status:", Status),
  hoverinfo = "text"
) %>%
  layout(
    title = "Change in Food Distribution (2020 vs 2019)",
    xaxis = list(title = "Locality"),
    yaxis = list(title = "Change in Pounds of Food Distributed"),
    barmode = "group"
  )

#house hold

household_summary <- data %>%
  group_by(Locality, Year) %>%  # Group by locality and year
  summarize(Totalhouseholdsserved = sum(HouseholdsServed, na.rm = TRUE))  # Replace FoodDistributed with your column name

house_comparison <- household_summary %>%
  group_by(Locality) %>%
  summarize(
    ChangeInhouseserved = sum(Totalhouseholdsserved[Year == 2020]) - 
      sum(Totalhouseholdsserved[Year == 2019])
  )
housecomparison <- house_comparison %>%
  mutate(
    Status = ifelse(ChangeInhouseserved > 0, "Increase", "Decrease")
  )

plot_ly(
  data = housecomparison,
  x = ~Locality,
  y = ~ChangeInhouseserved,
  color = ~Status,
  type = "bar",
  text = ~paste("Locality:", Locality, "<br>Change:", ChangeInhouseserved, "<br>Status:", Status),
  hoverinfo = "text"
) %>%
  layout(
    title = "Change in house hold served (2020 vs 2019)",
    xaxis = list(title = "Locality"),
    yaxis = list(title = "Change in houseof Food Distributed"),
    barmode = "group"
  )

text_counts <- table(comparison$Status)

# View the result
print(text_counts)

#heat map #heacount()t map 

data_2019_food <- food_distribution_summary %>%
  filter(Year == 2019)  # Replace 'Year' with the actual column name for years

# Create the density map analysis by years
#2019
leaflet(data_2019_food) %>%
  addTiles() %>%  # Add base map
  addHeatmap(
    lng = ~data_2019$Longitude,  # Replace with the actual longitude column name
    lat = ~data_2019$Latitude,   # Replace with the actual latitude column name
    intensity = ~data_2019_food$TotalFoodDistributed, # Replace with the column that indicates intensity, e.g., households served
    blur = 20,         # Controls how much the heatmap blurs
    max = 0.05,        # Max intensity value for scaling
    radius = 15        # Radius of influence for each point
  ) %>%
  addCircleMarkers(
    lng = ~data_2019$Longitude,  # Longitude for marker placement
    lat = ~data_2019$Latitude,   # Latitude for marker placement
    radius = 2,        # Size of the marker
    color = "#4d4d4d",    # Color of the marker
    fillOpacity = 0.7, # Opacity of the marker
    opacity = 0.2,
    popup = ~paste("Location: ", Locality, "<br>Households Served: ", TotalFoodDistributed, "<br>Year:", Year), # Show the location name in the popup when clicked
  )%>%
  setView(lng = mean(data$Longitude), lat = mean(data$Latitude), zoom = 7)

data_2020 <- data %>%
  filter(Year == 2020) 

#2020
leaflet(data_2020) %>%
  addTiles() %>%  # Add base map
  addHeatmap(
    lng = ~data_2020$Longitude,  # Replace with the actual longitude column name
    lat = ~data_2020$Latitude,   # Replace with the actual latitude column name
    intensity = ~data_2020$HouseholdsServed, # Replace with the column that indicates intensity, e.g., households served
    blur = 20,         # Controls how much the heatmap blurs
    max = 0.05,        # Max intensity value for scaling
    radius = 15        # Radius of influence for each point
  ) %>%
  addCircleMarkers(
    lng = ~data_2020$Longitude,  # Longitude for marker placement
    lat = ~data_2020$Latitude,   # Latitude for marker placement
    radius = 2,        # Size of the marker
    color = "#4d4d4d",    # Color of the marker
    fillOpacity = 0.7, # Opacity of the marker
    opacity = 0.2,
    popup = ~paste("Location: ", Locality, "<br>Households Served: ", HouseholdsServed, "<br>Year:", Year), 
  )%>%
  setView(lng = mean(data$Longitude), lat = mean(data$Latitude), zoom = 7)

# 2 ends

# research question 3

monthly_summary <- data %>%
  group_by(Year, Month) %>%
  summarize(TotalFoodDistributed = sum(PoundsFoodDistributed, na.rm = TRUE))
plot_ly(
  data = monthly_summary,
  x = ~Month,
  y = ~TotalFoodDistributed,
  color = ~as.factor(Year),  # Color-coded by Year
  type = 'scatter',
  mode = 'lines+markers',
  hoverinfo = 'text',
  text = ~paste(
    "Year:", Year, "<br>Month:", Month, "<br>Total Food Distributed:", TotalFoodDistributed
  )
)%>%
  layout(
    title = "Seasonal Trends in Food Distribution",
    xaxis = list(title = "Month"),
    yaxis = list(title = "Total Food Distributed"),
    legend = list(title = list(text = "Year")),
    hovermode = "closest"
  )

#seasons
data$Season <- case_when(
  data$Month %in% c("December", "January", "February") ~ "Winter",
  data$Month %in% c("March", "April", "May") ~ "Spring",
  data$Month %in% c("June", "July", "August") ~ "Summer",
  data$Month %in% c("September", "October", "November") ~ "Fall"
)

# Aggregate data by Year and Season
seasonal_summary <- data %>%
  group_by(Year, Season) %>%
  summarize(TotalFoodDistributed = sum(PoundsFoodDistributed, na.rm = TRUE))

plot_ly(
  data = seasonal_summary,
  x = ~Season,
  y = ~TotalFoodDistributed,
  color = ~as.factor(Year),  # Different colors for each year
  type = 'bar',
  text = ~paste(
    "Year:", Year, 
    "<br>Season:", Season, 
    "<br>Total Food Distributed:", TotalFoodDistributed
  ),
  hoverinfo = 'text'
)%>%
  layout(
    title = "Seasonal Trends in Food Distribution",
    xaxis = list(title = "Season"),
    yaxis = list(title = "Total Food Distributed"),
    barmode = "group",  # Group bars side-by-side
    legend = list(title = list(text = "Year"))
  )

# univarient
food_type_barplot <- plot_ly(data, y = ~IndividualsServed, type = 'bar', 
                             color = ~IndividualsServed, colors = 'Set1') %>%
  layout(title = 'Individuals served through food banks',
         xaxis = list(title = 'Indiviudal'),
         yaxis = list(title = 'Count'))
food_type_barplot

locality_barplot <- plot_ly(data, y = ~Locality, type = 'bar', 
                              color = ~Locality, colors = c('blue', 'red')) %>%
  layout(title = 'Locality distribution',
         xaxis = list(title = 'Locality'),
         yaxis = list(title = 'Count'))
donor_type_barplot

locality_barplot

