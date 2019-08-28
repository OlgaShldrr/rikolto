library(readr)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(readxl)

data <- read_csv("data/map_dataset.csv")

data$gender <- gsub(data$gender, pattern="0", replacement="Male")
data$gender <- gsub(data$gender, pattern="1", replacement="Female")

data$org <- gsub(data$org, pattern="koperasi_", replacement = "")
data$org <- gsub(data$org, pattern="organisasi_", replacement = "")
data$org <- gsub(data$org, pattern="oraganisasi_", replacement = "")
data$org <- gsub(data$org, pattern="lembaga_", replacement = "")
data$org <- gsub(data$org, pattern="koerintji_barokah_bersama", replacement = "barokah")
data$org <- gsub(data$org, pattern="_", replacement = " ")

data <- data %>%
  mutate(icon = case_when(
    comm == "coffee" ~ "1",
    comm == "cacao" ~ "2",
    comm == "cinnamon" ~ "3",
    comm == "rice" ~ "4"))

my_icons2 <- iconList(
  coffee <- makeIcon(iconUrl = "https://camo.githubusercontent.com/e8783c1b9fc99532bedbab3b8df9ce1b03d6f70b/68747470733a2f2f7777772e6272696172636c6966662e6564752f6d656469612f3339343033372f6d61726b65722e706e67",
                     iconWidth = 25, iconHeight = 25),
  cacao <- makeIcon(iconUrl = "https://bonnicilawgroup.com/wp-content/uploads/2015/08/map-marker-icon.png",
                     iconWidth = 25, iconHeight = 25),
  cinnamon <- makeIcon(iconUrl = "http://www.myiconfinder.com/uploads/iconsets/256-256-a5485b563efc4511e0cd8bd04ad0fe9e.png",
                       iconWidth = 25, iconHeight = 25),
  rice <- makeIcon(iconUrl = "https://childdevelopment.com.au/wp-content/uploads/map-marker-icon-300x300.png",
                       iconWidth = 25, iconHeight = 25)
)

# using original data_all dataframe
# leaflet using addCirleMarkers and addMArkers
map <- data %>% 
  leaflet() %>% 
  addTiles() %>% 
  # specific circle color according to the 'color' column
  #addCircleMarkers(lng = ~ lon, lat = ~ lat, color = ~ color, fillColor = ~ color, opacity = 0.8, radius = 15, fillOpacity = 0.8) %>% 
  # specific icon shape according to the 'Group' column
  addMarkers(lng = ~ lon, lat = ~ lat, icon = ~ my_icons2[as.numeric(icon)], 
             popup = ~ paste0("Commodity = ", comm,
                              "; Organization = ", org,
                              "; Gender = ", gender,
                              "; Productivity = ", round(as.numeric(productivity))))
#write.csv(data, "data/map_dataset.csv", row.names = FALSE)
