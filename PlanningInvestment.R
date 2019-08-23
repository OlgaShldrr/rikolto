drivers <- data %>% 
  select(contains("130"), commodity) %>% 
  group_by(commodity) %>% 
  summarise(land=sum(!is.na(data$`_4/prod_planning_130/land`)), weather = sum(!is.na(data$`_4/prod_planning_130/weather`)))

            