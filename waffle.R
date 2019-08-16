library(waffle)
library(extrafont)
library(tidyverse)
library(readxl)
library(flexdashboard)
data <- read_csv("data/data.csv")
counts <- data %>% 
  group_by(commodity, `_0/sex_012`) %>% 
  summarise(n=n()) %>% 
  mutate(freq = round((n / sum(n))*100))

counts <- na.omit(counts)

count <- as.vector(counts$freq)

waffle(c(Male = count[1], Female = count[2], Uknown = count[3]), rows = 5, title = "Gender Distribution of Farmers", colors = c("#9eab05","#c99700", "#9595d2"))


#facets by type of crop
library(tidyverse)
library(hrbrthemes)
library(waffle)


tibble(
  parts = factor(rep(month.abb[1:3], 3), levels=month.abb[1:3]),
  values = c(10, 20, 30, 6, 14, 40, 30, 20, 10),
  fct = c(rep("Thing 1", 3), rep("Thing 2", 3), rep("Thing 3", 3))
) -> xdf
library(ggthemes)
waffle <- ggplot(counts, aes(fill=`_0/sex_012`, values=freq)) +
  geom_waffle(color = "white", size=1.125, n_rows = 5) +
  facet_wrap(~commodity, ncol=1) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(
    title = "Farmers by gender and type of commodity"
  ) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle()

counts_age <- data %>% 
  group_by(commodity,`_0/age_011`) %>% 
  summarise(n=n()) %>% 
  mutate(freq = round((n / sum(n))*100)) %>% 
  na.omit()
subset_age <- data %>% 
  select(commodity, `_0/age_011`)
subset_age <- na.omit(subset_age)
age <- ggplot(subset_age, aes(commodity, fill = as.factor(`_0/age_011`))) + geom_bar()+
  labs(title = "Farmers by age and type of commodity", x = "Types of Commodity", y = "Count of Farmers")


#graph by production type and commodity
subset_prod_org <- data %>% 
  select(prod_type_006, contains("farmer_org"))
subset_prod_org <- na.omit(subset_prod_org)
prod_org <-ggplot(subset_prod_org, aes(`_0/farmer_org_014`, fill = as.factor(`prod_type_006`))) + geom_bar()+
  labs(title = "Production type by organization and by commodity", x = "Types of Commodity", y = "Count of Farmers")
library(plotly)
ggplotly(prod_org)
