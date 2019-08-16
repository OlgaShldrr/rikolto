library(ggmap)
library(readr)
library(tidyverse)

library(leaflet)
library(htmlwidgets)
library(readxl)
#register_google(Sys.getenv("GoogleAPI"))
#
##data import and wrangling coffee--------------------
#
#coffee <- read_delim("data/01 CSV Legacy - KOPI ALL.csv", 
#                                      ";", escape_double = FALSE, na = "n/a", trim_ws = TRUE)
#coffee <- coffee %>% 
#  select(country_002, starts_with("work_area"), `_0/village_010`, `_3/crop_productivity_104_cof`, `_0/farmer_org_014`, `_0/sex_012`) 
#coffee$map <- paste(coffee$country_002, coffee$work_area_province, coffee$work_area_ntt, coffee$work_area_sulsel, coffee$work_area_jambi, coffee$`_0/village_010`, sep = ", ")
#coffee$map <- gsub(x=coffee$map, pattern="NA, ", replacement = "")
#coordinates <- geocode(coffee$map)
#coffee <- cbind(coffee, coordinates)
#coffee <- coffee %>% 
#  select(productivity =`_3/crop_productivity_104_cof`, gender = `_0/sex_012`, map = map, lon, lat, org = `_0/farmer_org_014`) %>% 
#  mutate(comm = c("coffee"))
#
##data import and wrangling cacao--------------------
#cacao <- read_excel("data/02 CSV Legacy - KAKAO ALL.xlsx")                                      
#
#cacao <- cacao %>% 
#  select(country_002, starts_with("work_area"), `_0/village_010`, `_3/crop_productivity_104_coc`, `_0/farmer_org_014`, `_0/sex_012`) 
#cacao$map <- paste(cacao$country_002, cacao$work_area_province, cacao$work_area_ntt, cacao$work_area_sulsel, cacao$work_area_sulbar, cacao$`_0/village_010`, sep = ", ")
#cacao$map <- gsub(x=cacao$map, pattern="n/a, ", replacement = "")
#coordinates <- geocode(cacao$map)
#cacao <- cbind(cacao, coordinates)
#cacao <- cacao %>% 
#  select(productivity =`_3/crop_productivity_104_coc`, gender = `_0/sex_012`, map = map, lon, lat, org = `_0/farmer_org_014`) %>% 
#  mutate(comm = c("cacao"))
#
##data import and wrangling cinnamon--------------------
#cinnamon <- read_delim("data/03 CSV Legacy - KULIT MANIS - ALL.csv", 
#                                             ";", escape_double = FALSE, trim_ws = TRUE)
#cinnamon <- cinnamon %>% 
#  select(country_002, starts_with("work_area"), `_0/village_010`, `_3/crop_productivity_104_cin`, `_0/farmer_org_014`, `_0/sex_012`) 
#cinnamon$map <- paste(cinnamon$country_002, cinnamon$work_area_province, cinnamon$work_area_jambi, cinnamon$`_0/village_010`, sep = ", ")
#cinnamon$map <- gsub(x=cinnamon$map, pattern="n/a, ", replacement = "")
#coordinates <- geocode(cinnamon$map)
#cinnamon <- cbind(cinnamon, coordinates)
#cinnamon <- cinnamon %>% 
#  select(productivity =`_3/crop_productivity_104_cin`, gender = `_0/sex_012`, map = map, lon, lat, org = `_0/farmer_org_014`) %>% 
#  mutate(comm = c("cinnamon"))
#
##data import and wrangling rice--------------------
#rice <- read_delim("data/04 CSV Legacy - BERAS - ALL.csv", 
#                                       ";", escape_double = FALSE, trim_ws = TRUE)
#
#rice <- rice %>% 
#  select(country_002, starts_with("work_area"), `_0/village_010`, contains("productivity"), -contains("display"),`_0/farmer_org_014`, `_0/sex_012`) 
#rice$map <- paste(rice$country_002, rice$work_area_province, rice$work_area_jabar, rice$work_area_jateng, rice$`_0/village_010`, sep = ", ")
#rice$map <- gsub(x=rice$map, pattern="n/a, ", replacement = "")
#coordinates <- geocode(rice$map)
#rice <- cbind(rice, coordinates)
#rice <- rice %>% 
#  select(productivity =`_3/VARITAS_BERAS_HITAM/crop_productivity_104_rice_bh`, gender = `_0/sex_012`, map = map, lon, lat, org = `_0/farmer_org_014`) %>% 
#  mutate(comm = c("rice"))
#
##merging datasets---------------
#data <- rbind(coffee, cacao)
#data <- rbind(data,cinnamon)
#data <- rbind(data, rice)

data <- read_csv("data/map_dataset.csv")

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
