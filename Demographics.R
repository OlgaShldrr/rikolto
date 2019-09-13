library(waffle)
library(extrafont)
library(tidyverse)
library(readxl)
library(flexdashboard)
data <- read_csv("data/data.csv")
counts <- data %>% 
  select(Commodity=commodity, Gender=sex) %>% 
  group_by(Commodity, Gender) %>% 
  summarise(`Number of farmers`=n()) %>% 
  mutate(`Percent,%` = round((`Number of farmers` / sum(`Number of farmers`))*100))

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
age <- ggplot(subset_age, aes(commodity, fill = age)) +
  geom_bar()+
  labs(x = "Types of Commodity", y = "Count of Farmers") +scale_fill_manual(values=sample(colors))


#graph by production type and commodity
subset_prod_org <- data %>% 
  select(`Production type`, commodity)
subset_prod_org <- na.omit(subset_prod_org)
prod_org <- ggplot(subset_prod_org, aes(commodity, fill = `Production type`)) + geom_bar()+
  labs(title = "Number of farmers using different production types by commodity", x = "Commodity", y = "Count of Farmers")+
  scale_fill_manual(values=sample(colors))
library(plotly)
ggplotly(prod_org)
