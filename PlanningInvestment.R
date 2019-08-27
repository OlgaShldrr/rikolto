library(tidyverse)

data <- read_csv("data/data.csv")

drivers <- data %>% 
  select(contains("130"), commodity)
drivers <- drivers %>% 
  gather(., key="drivers", value = "value", -commodity)
drivers$value <- gsub(drivers$value,pattern = "land", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "irrigation", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "weather", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "climate", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "price", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "demand", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "capital", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "labour", replacement = 1)
drivers <- drivers %>% 
  filter(value==1)
drivers_counts <- drivers %>% 
  filter(!is.na(value)) %>% 
  group_by(commodity, drivers) %>% 
  summarise(count=n())
drivers_plot <- ggplot(drivers_counts, aes(fill=drivers, y=count, x=commodity)) + 
  geom_bar(position="fill", stat="identity")

ggplotly(drivers_plot)


access <- data %>% 
  select(contains("091"), commodity) 
access <- access %>% 
  gather(., key="access", value = "value", -commodity)
access <- access %>% 
  filter(!is.na(value))
access_counts <- access %>% 
  filter(!is.na(value)) %>% 
  group_by(commodity, access) %>% 
  summarise(count=n()) %>% 
  mutate(fraction = count / sum(count))
access_counts <- access_counts %>% 
  mutate(ymax = cumsum(fraction))
access_counts <- access_counts %>% 
  mutate(ymin = c(0, head(ymax, n=-1)))

access_plot <- ggplot(access_counts, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=access)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) 
#+
#  facet_grid(~ commodity)# Try to remove that to see how to make a pie chart



why_no_acess <- data %>% 
  select(contains("092"), commodity) 
why_no_acess <- why_no_acess %>% 
  gather(., key="noaccess", value = "value", -commodity)
why_no_acess <- why_no_acess %>% 
  filter(!is.na(value)) %>% 
  group_by(commodity, noaccess) %>% 
  summarise(count=n()) %>% 
  mutate(fraction = round(count / sum(count)*100, digits=2))

investment <- data %>% 
  select(contains("131"), commodity) 
investment <- investment %>% 
  gather(., key="investment", value = "value", -commodity)
investment_counts <- investment %>% 
  filter(!is.na(value)) %>% 
  group_by(investment) %>% 
  summarise(count=n()) %>% 
  mutate(fraction = count / sum(count))
k <- ggplot(investment_counts, aes(x=investment, y=count)) +
  geom_segment( aes(x=investment, xend=investment, y=0, yend=count)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 
ggplotly(k)
