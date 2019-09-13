library(readxl)
library(tidyverse)
library(stringr)
library(ggmap)
library(naniar)

data <- read_excel("data/data.xlsx")
subset <- data %>% 
  select(starts_with("commodity")) 

subset$commodity <- paste(subset$commodity_005_g,subset$commodity_005_c, subset$commodity_005_g,
                          subset$commodity_005_c, subset$commodity_005_p, subset$'commodity_005_f/vegetables', 
                          subset$'commodity_005_f/goldenberry',subset$'commodity_005_f/banana',     
                          subset$'commodity_005_f/tomato',subset$'commodity_005_f/chili',      
                          subset$'commodity_005_f/carrot',subset$'commodity_005_f/onion',      
                          subset$'commodity_005_f/cabbage',subset$'commodity_005_f/lettuce',sep=", ")
subset$commodity <- gsub(x=subset$commodity, pattern="NA*", replacement = "", perl = TRUE)
subset$commodity <- gsub(x=subset$commodity, pattern=",*", replacement = "", perl = TRUE)
subset$commodity <- str_squish(subset$commodity)
commodity <- as.data.frame(str_split_fixed(subset$commodity, " ", 2))
commodity$V1 <- gsub(x=commodity$V1, pattern="rice", replacement = "rice", fixed = FALSE)
types_of_rice <- c("rice_ir64","rice_red_ir64","rice_whitelocal",
                   "rice_whitelocal_ir64","rice_red_whitelocal","rice_black_ir64",
                   "rice_red_black_ir64","rice_black","rice_black_whitelocal",
                   "rice_red")
for (i in 1:length(types_of_rice)) {
  commodity$V1 <- gsub(x=commodity$V1, pattern=types_of_rice[i],replacement = "rice")
}

data$commodity <- commodity$V1
unique(data$commodity)
for (i in 1:length(types_of_rice)) {
  commodity$V1 <- gsub(x=commodity$V1, pattern=types_of_rice[i],replacement = "rice")
}

data$commodity <- commodity$V1
unique(data$commodity)



productivity <- data %>% 
  select(contains("productivity"), -contains("OUTLIER"), -contains("DISPLAY"))
productivity$x <- paste(productivity$`_3/crop_productivity_105_rice`,productivity$`_3/crop_productivity_105_ses`, productivity$`_3/crop_productivity_105_pul`, 
           productivity$`_3/crop_productivity_105_coc`,  productivity$`_3/crop_productivity_105_cof`,  productivity$`_3/crop_productivity_105_cin`,
          productivity$`_3/crop_productivity_105_veg`,  productivity$`_3/crop_productivity_105_gb`,  productivity$`_3/crop_productivity_105_ba`, 
          productivity$`_3/crop_productivity_105_to`,   productivity$`_3/crop_productivity_105_chi`,  productivity$`_3/crop_productivity_105_ca`,  
          productivity$`_3/crop_productivity_105_on`,   productivity$`_3/crop_productivity_105_cab`,  productivity$`_3/crop_productivity_105_let` , sep = ", ")
productivity$x <- gsub(productivity$x, pattern="NA, ", replacement = "")
productivity$x <- gsub(productivity$x, pattern=", NA", replacement = "")
productivity$x <- round(as.numeric(productivity$x), digits = 2)
data$productivity <- productivity$x

farmland <- data %>%  select(contains("dedicated_farmland"), -contains("OUTLIER"))
farmland$x <- paste(farmland$`_3/dedicated_farmland_102_rice`,
                    farmland$`_3/dedicated_farmland_102_ses`,
                    farmland$`_3/dedicated_farmland_102_pul`,
                    farmland$`_3/dedicated_farmland_102_coc`,
                    farmland$`_3/dedicated_farmland_102_cof`,
                    farmland$`_3/dedicated_farmland_102_cin`,
                    farmland$`_3/dedicated_farmland_102_veg`,
                    farmland$`_3/dedicated_farmland_102_gb`,
                    farmland$`_3/dedicated_farmland_102_ba` ,
                    farmland$`_3/dedicated_farmland_102_to`,
                    farmland$`_3/dedicated_farmland_102_chi`,
                    farmland$`_3/dedicated_farmland_102_ca`,
                    farmland$`_3/dedicated_farmland_102_on` ,
                    farmland$`_3/dedicated_farmland_102_cab`,
                    farmland$`_3/dedicated_farmland_102_let`, sep=", ")
farmland$x <- gsub(farmland$x, pattern="NA, ", replacement = "")
farmland$x <- gsub(farmland$x, pattern=", NA", replacement = "")
farmland$x <- round(as.numeric(farmland$x), digits=2)
data$farmland <- farmland$x

sold <- data %>% 
  select(contains("107"), -contains("OUTLIER")) %>% 
  transmute(sold_107_rice = data$`_3/sold_formal_fo_107_rice`+data$`_3/sold_formal_com_107_rice`,
            sold_107_coffee = data$`_3/sold_formal_fo_107_cof`,
            sold_107_cocoa= data$`_3/sold_formal_fo_107_coc`,
            sold_107_cinnamon=data$`_3/sold_formal_fo_107_cin`)

sold$x <- paste(sold$sold_107_rice, sold$sold_107_coffee, sold$sold_107_cocoa, sold$sold_107_cinnamon, sep=", ")
sold$x <- gsub(sold$x, pattern="NA, ", replacement = "")
sold$x <- gsub(sold$x, pattern=", NA", replacement = "")
sold$x <- round(as.numeric(sold$x), digits=2)
data$sold <- sold$x

production <- data %>% 
  select(contains("production"), -contains("OUTLIER"), -contains("invest"))
production$x<-paste(production$`_3/crop_production_103_rice`, production$`_3/crop_production_103_ses`, production$`_3/crop_production_103_pul`, production$`_3/crop_production_103_coc`, 
                    production$`_3/crop_production_103_cof`, production$`_3/crop_production_103_cin`, production$`_3/crop_production_103_veg`, production$`_3/crop_production_103_gb`,
                    production$`_3/crop_production_103_ba`, production$`_3/crop_production_103_to`, production$`_3/crop_production_103_chi`, production$`_3/crop_production_103_ca`, 
                    production$`_3/crop_production_103_on`, production$`_3/crop_production_103_cab`, production$`_3/crop_production_103_let`, sep=", ")
production$x <- gsub(production$x, pattern="NA, ", replacement = "")
production$x <- gsub(production$x, pattern=", NA", replacement = "")
production$x <- round(as.numeric(production$x), digits=2)
data$production <- production$x

data$share_commercialized <- data$sold/data$production
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="koperasi_", replacement = "")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="organisasi_", replacement = "")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="oraganisasi_", replacement = "")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="lembaga_", replacement = "")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="koerintji_barokah_bersama", replacement = "barokah")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="_", replacement = " ")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="simpatik", replacement = "msa")

data$`farmer organisation` <- data$`_0/farmer_org_014`

data$`farmer organisation`[is.na(data$`farmer organisation`)] <- "comparison\n group"
data$`farmer organisation` <- as.factor(data$`farmer organisation`)

data$commodity <- as.character(data$commodity)
data$commodity[data$`farmer organisation`=="comparison\n group"] <- "comparison \n commodity"
data$commodity <- as.factor(data$commodity)

#data import and wrangling coffee--------------------

coffee <- read_delim("data/01 CSV Legacy - KOPI ALL.csv", 
                                      ";", escape_double = FALSE, na = "n/a", trim_ws = TRUE)
coffee <- coffee %>% 
  separate(col = store_gps, into=c("lat","lon","gps3","gps4"),sep= " ")

coffee <- coffee %>% 
  select(`_uuid`, lat, lon)
#data import and wrangling cacao--------------------
cacao <- read_excel("data/02 CSV Legacy - KAKAO ALL.xlsx")                                      

cacao <- cacao %>%
  separate(col = store_gps, into=c("lat","lon","gps3","gps4"),sep= " ")

cacao <- cacao %>% 
  select(`_uuid`, lat, lon)

#data import and wrangling cinnamon--------------------
cinnamon <- read_delim("data/03 CSV Legacy - KULIT MANIS - ALL.csv", 
                                             ";", escape_double = FALSE, trim_ws = TRUE)
cinnamon <- cinnamon %>% 
  separate(col = store_gps, into=c("lat","lon","gps3","gps4"),sep= " ")

cinnamon <- cinnamon %>% 
  select(`_uuid`, lat, lon)

#data import and wrangling rice--------------------
rice <- read_delim("data/04 CSV Legacy - BERAS - ALL.csv", 
                                       ";", escape_double = FALSE, trim_ws = TRUE)

rice <- rice %>% 
  separate(col = store_gps, into=c("lat","lon","gps3","gps4"),sep= " ")

rice <- rice %>% 
  select(`_uuid`, lat, lon)

raw <- rbind(rice, coffee)
raw <- rbind(raw, cacao)
raw <- rbind(raw, cinnamon)
data <- merge(data, raw)

#merging datasets---------------
data$productivity <- as.numeric(data$productivity)
data$farmland <- as.numeric(data$farmland)

data$age <- data$`_0/age_011`
data$age <- gsub(data$age, pattern="1", replacement = "Less than or equal to 35 years old")
data$age <- gsub(data$age, pattern="0", replacement = "Over 35 years old")

data$sex <- data$`_0/sex_012`
data$sex <- gsub(data$sex, pattern="0", replacement="Male")
data$sex <- gsub(data$sex, pattern="1", replacement="Female")

data$prod_type_006 <- as.factor(data$prod_type_006)
data$age <- as.factor(data$age)
data$sex <- as.factor(data$sex)


data <- data %>%
  mutate(icon = case_when(
    commodity == "coffee" ~ "1",
    commodity == "cocoa" ~ "2",
    commodity == "cinnamon" ~ "3",
    commodity == "rice" ~ "4",
    commodity == "comparison \n commodity" ~ "5"))


data$id = row.names(data)
# using original data_all dataframe
# leaflet using addCirleMarkers and addMArkers

data <- data %>% 
replace_with_na(replace = list(lat = "n/a", lon = "n/a"))

data$lat <- as.numeric(data$lat)
data$lon <- as.numeric(data$lon)

data$`Production type` <- data$prod_type_006
write.csv(data, "data/data.csv", row.names = FALSE)
