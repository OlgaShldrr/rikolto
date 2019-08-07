library(readxl)
data <- read_excel("data/data.xlsx")
suppressPackageStartupMessages(library(tidyverse))

a <- tibble(a=runif(4),b=letters[1:4], p=list(seq(.01,1,.01))) %>%
  unnest() %>%
  mutate(q = ifelse(a>p,1,0),
         label = paste(b,scales::percent(a)),
         x = ((p-0.01)*100)%%10,
         y = ((p-0.01)*100)%/%10) 
  ggplot() +
  geom_point(mapping=aes(x=x, y=y, color = q, fill=q, shape = factor(q))) +
  scale_y_continuous(name="") +
  scale_x_continuous(name="") +
  scale_shape_manual(values = c(25,24)) +
  guides(fill=FALSE, shape=FALSE, color=FALSE) +
  facet_wrap(~label, nrow = 2) +
  theme_void()
data <- data %>% 
  select(`_0/sex_012`) %>% 
  mutate(p=list(seq(.01,1,.01))) %>%
  unnest() %>% 
  mutate(x = ((p-0.01)*100)%%10,
         y = ((p-0.01)*100)%/%10)

data %>% 
  ggplot() +
  geom_point(mapping=aes(x=x, y=y, color=`_0/sex_012`)) +
  scale_y_continuous(name="") +
  scale_x_continuous(name="") +
  scale_shape_manual(values = c(25,24)) +
  guides(fill=FALSE, shape=FALSE, color=FALSE) +
  theme_void()

data %>% 
  group_by(`_0/sex_012`) %>% 
  summarise(count=n())
