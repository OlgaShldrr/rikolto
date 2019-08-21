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

