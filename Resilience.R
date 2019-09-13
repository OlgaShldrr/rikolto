info <- data %>% 
  select(contains("info"), -contains("informal"), commodity) %>% 
  group_by(commodity) %>% 
  summarise_all(funs(mean)) %>%
  na.omit()

info <- info %>% 
  gather(., key="info", value="value", -commodity)
info$info <- gsub(info$info, pattern = "_1/info_crop_096", replacement = "information\n on cropping and\n livestock practices")    
info$info <- gsub(info$info, pattern = "_1/info_market_097", replacement = "market information") 
info$info <- gsub(info$info, pattern = "_1/info_weather_098", replacement = "information\n on weather\n patterns") 
info$info <- gsub(info$info, pattern = "_1/info_health_099", replacement = "information\n on health\n and nutrition")

info_plot <- ggplot(info, aes(fill=commodity, y=value, x=info)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=sample(colors))+
  labs(x="Types of information", y="Average frequency of access")

ggplotly(info_plot)

shock <- data %>% 
  select(contains("shock"), Commodity = commodity, `Farmer organisation` = `farmer organisation`) %>% 
  group_by(Commodity, `Farmer organisation`) %>%
  summarise_all(funs(mean))

shock <- shock %>% 
  gather(., key="Shock", value="Average \n intensity/frequency", -Commodity, -`Farmer organisation`)
shock$Shock <- gsub(shock$Shock, pattern = "_1/shock_soc_080", replacement = "Social shocks")  
shock$Shock <- gsub(shock$Shock, pattern = "_1/shock_econ_081", replacement = "Economic shocks") 
shock$Shock <- gsub(shock$Shock, pattern = "_1/shock_env_082", replacement = "Environmental shocks")
shock$`Average\n intensity/frequency` <- round(shock$`Average \n intensity/frequency`, digits=2)
shock <- shock %>% 
  select(Commodity, `Farmer organisation`, Shock, `Average
 intensity/frequency`)
strat <- data %>% 
  select(contains("strat"), commodity)  %>% 
  na.omit()

strat <- strat %>% 
  gather(., key="strat", value="value", -commodity)
strat$strat <- gsub(strat$strat, pattern = "_1/strat_migr_083", replacement = "Migration") 
strat$strat <- gsub(strat$strat, pattern = "_1/strat_aid_084", replacement = "External aid")  
strat$strat <- gsub(strat$strat, pattern = "_1/strat_inc_085", replacement = "Looking for new \n sources of income") 
strat$strat <- gsub(strat$strat, pattern = "_1/strat_exp_086", replacement = "Reducing my household\n  expenses")  
strat$strat <- gsub(strat$strat, pattern = "_1/strat_sav_087", replacement = "Using savings to make\n up for the shortfall")

library(hrbrthemes)
strategy <- ggplot(data=strat, aes(x=value, fill=commodity)) +
  geom_density(adjust=1.5) +
  facet_wrap(~strat) +
  scale_fill_manual(values=sample(colors))+
  labs(x="")

ggplotly(strategy)
