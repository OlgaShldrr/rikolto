library(waffle)
library(extrafont)
library(tidyverse)
library(readxl)
library(flexdashboard)
data <- read_excel("data/data.xlsx")
counts <- data %>% 
  group_by(`_0/sex_012`) %>% 
  summarise(n=n()) %>% 
  mutate(freq = round((n / sum(n))*100))
font_import()

y
y
# check that Font Awesome is imported
fonts()[grep("Awesome", fonts())]
# [1] "FontAwesome"
# this should be fine for Mac OSX
loadfonts()
# use this if things look odd in RStudio under Windows
loadfonts(device = "win")
count <- as.vector(counts$freq)

waffle(c(Male = count[1], Female = count[2], Uknown = count[3]), rows = 5, title = "Gender Distribution of Farmers")
waffle(c(Male = count[1], Female = count[2], Uknown = count[3]), rows = 5, use_glyph = "fa-child", glyph_size = 6, 
       title = "Look I made an infographic using R!")

iron(
  waffle(c(Male = count[1], Female = count[2], Uknown = count[3]), rows = 5, use_glyph = "car", glyph_size = 6, 
         colors = c("#c7d4b6", "#a3aabd"), title = "Country A"),
  waffle(c(Male = count[1], Female = count[2], Uknown = count[3]), rows = 5, use_glyph = "car", glyph_size = 6,
         colors = c("#c7d4b6", "#a3aabd"), title = "Country B")
)
