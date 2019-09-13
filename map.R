library(readr)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(readxl)

data <- read_csv("data/data.csv")
#
#data$gender <- gsub(data$gender, pattern="0", replacement="Male")
#data$gender <- gsub(data$gender, pattern="1", replacement="Female")
#
#data$org <- gsub(data$org, pattern="koperasi_", replacement = "")
#data$org <- gsub(data$org, pattern="organisasi_", replacement = "")
#data$org <- gsub(data$org, pattern="oraganisasi_", replacement = "")
#data$org <- gsub(data$org, pattern="lembaga_", replacement = "")
#data$org <- gsub(data$org, pattern="koerintji_barokah_bersama", replacement = "barokah")
#data$org <- gsub(data$org, pattern="_", replacement = " ")

#data <- data %>%
#  mutate(icon = case_when(
#    commodity == "coffee" ~ "1",
#    commodity == "cacao" ~ "2",
#    commodity == "cinnamon" ~ "3",
#    commodity == "rice" ~ "4"))

my_icons2 <- iconList(
  coffee <- makeIcon(iconUrl = "https://camo.githubusercontent.com/e8783c1b9fc99532bedbab3b8df9ce1b03d6f70b/68747470733a2f2f7777772e6272696172636c6966662e6564752f6d656469612f3339343033372f6d61726b65722e706e67",
                     iconWidth = 25, iconHeight = 25),
  cacao <- makeIcon(iconUrl = "https://bonnicilawgroup.com/wp-content/uploads/2015/08/map-marker-icon.png",
                    iconWidth = 25, iconHeight = 25),
  cinnamon <- makeIcon(iconUrl = "http://www.myiconfinder.com/uploads/iconsets/256-256-a5485b563efc4511e0cd8bd04ad0fe9e.png",
                       iconWidth = 25, iconHeight = 25),
  rice <- makeIcon(iconUrl = "https://childdevelopment.com.au/wp-content/uploads/map-marker-icon-300x300.png",
                   iconWidth = 25, iconHeight = 25),
  `comparison \n commodity` <- makeIcon(iconUrl = "http://www.pngall.com/wp-content/uploads/2017/05/Map-Marker-Free-PNG-Image.png",
                                     iconWidth = 25, iconHeight = 25)
)

data$id <- row.names(data)
map_subset <- data %>% 
  select(commodity, icon, lon, lat, `farmer organisation`, farmland, sex, age, id, productivity)
complete <- data %>% 
  select(commodity, icon, lon, lat) %>%
  na.omit()
map_subset <- merge(complete, map_subset, all.x=TRUE)

# using original data_all dataframe
# leaflet using addCirleMarkers and addMArkers
map <- map_subset %>% 
  leaflet() %>% 
  addTiles() %>% 
  # specific circle color according to the 'color' column
  #addCircleMarkers(lng = ~ lon, lat = ~ lat, color = ~ color, fillColor = ~ color, opacity = 0.8, radius = 15, fillOpacity = 0.8) %>% 
  # specific icon shape according to the 'Group' column
  addMarkers(lng = ~ lon, lat = ~ lat, icon = ~ my_icons2[as.numeric(icon)], 
             popup = ~ paste0("Commodity = ", map_subset$commodity,
                              "; Organization = ", map_subset$`farmer organisation`,
                              "; Gender = ", map_subset$sex,
                              "; Productivity = ", map_subset$productivity,
                              " ton; Farmland = ", map_subset$farmland,
                              " ha"))
#write.csv(data, "data/map_dataset.csv", row.names = FALSE)
