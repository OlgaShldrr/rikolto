library(readxl)
library(tidyverse)
library(stringr)
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
write.csv(data, "data/data.csv", row.names = FALSE)

#productivity <- data %>% 
#  select(contains("productivity"), -contains("OUTLIER"), -contains("DISPLAY"))
#productivity$x <- paste(productivity$`_3/crop_productivity_105_rice`,productivity$`_3/crop_productivity_105_ses`, productivity$`_3/crop_productivity_105_pul`, 
#           productivity$`_3/crop_productivity_105_coc`,  productivity$`_3/crop_productivity_105_cof`,  productivity$`_3/crop_productivity_105_cin`,
#          productivity$`_3/crop_productivity_105_veg`,  productivity$`_3/crop_productivity_105_gb`,  productivity$`_3/crop_productivity_105_ba`, 
#          productivity$`_3/crop_productivity_105_to`,   productivity$`_3/crop_productivity_105_chi`,  productivity$`_3/crop_productivity_105_ca`,  
#          productivity$`_3/crop_productivity_105_on`,   productivity$`_3/crop_productivity_105_cab`,  productivity$`_3/crop_productivity_105_let` , sep = ", ")
#productivity$x <- gsub(productivity$x, pattern="NA, ", replacement = "")
#productivity$x <- gsub(productivity$x, pattern=", NA", replacement = "")
#productivity$x <- as.numeric(productivity$x)
#data$productivity <- productivity$x
#
#write.csv(data, "data/data.csv", row.names = FALSE)

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
farmland$x <- as.numeric(farmland$x)
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
sold$x <- as.numeric(sold$x)
data$sold <- sold$x



data$share_commercialized <- data$sold/data$production
data$commodity <- as.factor(data$commodity)
write.csv(data, "data/data.csv", row.names = FALSE)

production <- data %>% 
  select(contains("production"), -contains("OUTLIER"), -contains("invest"))
production$x<-paste(production$`_3/crop_production_103_rice`, production$`_3/crop_production_103_ses`, production$`_3/crop_production_103_pul`, production$`_3/crop_production_103_coc`, 
                    production$`_3/crop_production_103_cof`, production$`_3/crop_production_103_cin`, production$`_3/crop_production_103_veg`, production$`_3/crop_production_103_gb`,
                    production$`_3/crop_production_103_ba`, production$`_3/crop_production_103_to`, production$`_3/crop_production_103_chi`, production$`_3/crop_production_103_ca`, 
                    production$`_3/crop_production_103_on`, production$`_3/crop_production_103_cab`, production$`_3/crop_production_103_let`, sep=", ")
production$x <- gsub(production$x, pattern="NA, ", replacement = "")
production$x <- gsub(production$x, pattern=", NA", replacement = "")
production$x <- as.numeric(production$x)
data$production <- production$x

write.csv(data, "data/data.csv", row.names = FALSE)

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

data$age <- data$`_0/age_011`
data$age <- gsub(data$age, pattern="1", replacement = "Less than or equal to 35 years old")
data$age <- gsub(data$age, pattern="0", replacement = "Over 35 years old")

data$sex <- data$`_0/sex_012`
data$sex <- gsub(data$sex, pattern="0", replacement="Male")
data$sex <- gsub(data$sex, pattern="1", replacement="Female")

data$prod_type_006 <- as.factor(data$prod_type_006)
data$age <- as.factor(data$age)
data$sex <- as.factor(data$sex)
data$`_0/farmer_org_014` <- as.factor(data$`_0/farmer_org_014`)

data <- read_csv("data/data.csv")

data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="koperasi_", replacement = "")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="organisasi_", replacement = "")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="oraganisasi_", replacement = "")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="lembaga_", replacement = "")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="koerintji_barokah_bersama", replacement = "barokah")
data$`_0/farmer_org_014` <- gsub(data$`_0/farmer_org_014`, pattern="_", replacement = " ")

write.csv(data, "data/data.csv", row.names = FALSE)
