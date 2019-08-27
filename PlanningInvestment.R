library(tidyverse)
drivers <- data %>% 
  select(contains("130"), commodity) %>% 
  group_by(commodity) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% 
  na.omit()


access_to_credit <- data %>% 
  select(contains("091"), commodity) %>% 
  group_by(commodity) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% 
  na.omit()

why_no_acess <- data %>% 
  select(contains("092"), commodity) %>% 
  group_by(commodity) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% 
  na.omit()

investment <-   data %>% 
  select(contains("131"), commodity) %>% 
  group_by(commodity) %>% 
  summarise_all(funs(sum(!is.na(.)))) %>% 
  na.omit()          
