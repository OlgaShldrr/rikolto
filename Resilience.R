info <- data %>% 
  select(contains("info"), -contains("informal"), commodity) %>% 
  group_by(commodity) %>% 
  summarise_all(funs(mean)) %>%
  na.omit()

shock <- data %>% 
  select(contains("shock"), commodity) %>% 
  group_by(commodity) %>%
  summarise_all(funs(n))
  
strat <- data %>% 
  select(contains("strat"), `_0/farmer_org_014`) %>% 
  group_by(`_0/farmer_org_014`) %>% 
  summarise_all(funs(mean))
