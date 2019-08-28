library(waffle)
library(extrafont)
library(tidyverse)
library(readxl)
library(flexdashboard)
data <- read_csv("data/data.csv")
counts <- data %>% 
  group_by(commodity, sex) %>% 
  summarise(n=n()) %>% 
  mutate(freq = round((n / sum(n))*100))

counts <- na.omit(counts)

count <- as.vector(counts$freq)



counts_age <- data %>% 
  group_by(commodity,age) %>% 
  summarise(n=n()) %>% 
  mutate(freq = round((n / sum(n))*100)) %>% 
  na.omit()
subset_age <- data %>% 
  select(commodity, age)
subset_age <- na.omit(subset_age)
age <- ggplot(subset_age, aes(commodity, fill = age)) + geom_bar()+
  labs(title = "Farmers by age and type of commodity", x = "Types of Commodity", y = "Count of Farmers") +scale_fill_manual(values=sample(colors))


#graph by production type and commodity
subset_prod_org <- data %>% 
  select(prod_type_006, contains("farmer_org"))
subset_prod_org <- na.omit(subset_prod_org)
prod_org <- ggplot(subset_prod_org, aes(`_0/farmer_org_014`, fill = `prod_type_006`)) + geom_bar()+
  labs(title = "Production type by organization and by commodity", x = "Types of Commodity", y = "Count of Farmers")+scale_fill_manual(values=sample(colors))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
library(plotly)
ggplotly(prod_org)
