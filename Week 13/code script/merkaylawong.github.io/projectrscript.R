library(leaflet)
library(shiny)
library(plotly)
library(tidyverse)
library(stringr)
library(readxl)
library(lubridate)

# Sample dataset with town names, coordinates, and median resale prices
housing_data1 = read.csv("mydataset.csv", header = TRUE, stringsAsFactors = TRUE)

na.omit(housing_data1)

housing_data1 %>%
  group_by(town) %>%
  summarize(rp=median(resale_price)) -> data

data %>%
  rename(Town = town)%>%
  rename(MedianResalePrice = rp) ->data
data %>% 
  mutate(Latitude = case_when(
    data$Town == "ANG MO KIO" ~ 1.3803,
    data$Town == "BEDOK" ~ 1.3236,
    data$Town == "BISHAN" ~ 1.3526,
    data$Town == "BUKIT BATOK" ~ 1.3590,
    data$Town == "BUKIT MERAH" ~ 1.2819,
    data$Town == "BUKIT PANJANG" ~ 1.3774,
    data$Town == "BUKIT TIMAH" ~ 1.3294,
    data$Town == "CENTRAL AREA" ~ 1.2789,
    data$Town == "CHOA CHU KANG" ~ 1.3840,
    data$Town == "CLEMENTI" ~ 1.3162,
    data$Town == "GEYLANG" ~ 1.3201,
    data$Town == "HOUGANG" ~ 1.3612,
    data$Town == "JURONG EAST" ~ 1.3329,
    data$Town == "JURONG WEST" ~ 1.3403,
    data$Town == "KALLANG/WHAMPOA" ~ 1.3100,
    data$Town == "MARINE PARADE" ~ 1.3020,
    data$Town == "PASIR RIS" ~ 1.3721,
    data$Town == "PUNGGOL" ~ 1.3984,
    data$Town == "QUEENSTOWN" ~ 1.3013,
    data$Town == "SEMBAWANG" ~ 1.4491,
    data$Town == "SENGKANG" ~ 1.3868,
    data$Town == "SERANGOON" ~ 1.3554,
    data$Town == "TAMPINES" ~ 1.3496,
    data$Town == "TOA PAYOH" ~ 1.3343,
    data$Town == "WOODLANDS" ~ 1.4382,
    data$Town == "YISHUN" ~ 1.4304
    
  )) %>%
  mutate(Longitude =case_when(
    data$Town == "ANG MO KIO" ~ 103.8397,
    data$Town == "BEDOK" ~ 103.9273,
    data$Town == "BISHAN" ~ 103.8352,
    data$Town == "BUKIT BATOK" ~ 103.7637,
    data$Town == "BUKIT MERAH" ~ 103.8239,
    data$Town == "BUKIT PANJANG" ~ 103.7719,
    data$Town == "BUKIT TIMAH" ~ 103.8021,
    data$Town == "CENTRAL AREA" ~ 103.8536,
    data$Town == "CHOA CHU KANG" ~ 103.7470,
    data$Town == "CLEMENTI" ~ 103.7649,
    data$Town == "GEYLANG" ~ 103.8198,
    data$Town == "HOUGANG" ~ 103.8863,
    data$Town == "JURONG EAST" ~ 103.7436,
    data$Town == "JURONG WEST" ~ 103.7090,
    data$Town == "KALLANG/WHAMPOA" ~ 103.8651,
    data$Town == "MARINE PARADE" ~ 103.8971,
    data$Town == "PASIR RIS" ~ 103.9474,
    data$Town == "PUNGGOL" ~ 103.9072,
    data$Town == "QUEENSTOWN" ~ 103.7981,
    data$Town == "SEMBAWANG" ~ 103.8185,
    data$Town == "SENGKANG" ~ 103.8914,
    data$Town == "SERANGOON" ~ 103.8679,
    data$Town == "TAMPINES" ~ 103.9568,
    data$Town == "TOA PAYOH" ~ 103.8563,
    data$Town == "WOODLANDS" ~ 103.7890,
    data$Town == "YISHUN" ~ 103.8354
    
  ))-> data


ui <- fluidPage(
  leafletOutput("map")
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(data = data) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~Latitude,
        lng = ~Longitude,
        radius = sqrt(data$MedianResalePrice) / 100,  # Adjust the radius based on the price
        fillOpacity = 0.5,
        color = "blue",
        stroke = FALSE,
        label = ~paste("Town: ", Town, "Median Resale Price: $", MedianResalePrice)
      ) %>%
      addProviderTiles("Stamen.Watercolor")  # You can choose different map styles  
    })
}

shinyApp(ui, server)

