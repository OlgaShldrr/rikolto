info <- data %>% 
  select(contains("info"), -contains("informal"), commodity) %>% 
  group_by(commodity) %>% 
  summarise_all(funs(mean)) %>%
  na.omit()

info <- info %>% 
  gather(., key="info", value="value", -commodity)
info$info <- gsub(info$info, pattern = "_1/info_crop_096", replacement = "information on cropping and livestock practices")    
info$info <- gsub(info$info, pattern = "_1/info_market_097", replacement = "market information") 
info$info <- gsub(info$info, pattern = "_1/info_weather_098", replacement = "information on weather patterns") 
info$info <- gsub(info$info, pattern = "_1/info_health_099", replacement = "information on health and nutrition")

info_plot <- ggplot(info, aes(fill=commodity, y=value, x=info)) + 
  geom_bar(position="dodge", stat="identity")+scale_fill_manual(values=sample(colors))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(info_plot)

shock <- data %>% 
  select(contains("shock"), commodity, `_0/farmer_org_014`) %>% 
  group_by(commodity, `_0/farmer_org_014`) %>%
  summarise_all(funs(mean))

shock <- shock %>% 
  gather(., key="shock", value="value", -commodity, -`_0/farmer_org_014`)
shock$shock <- gsub(shock$shock, pattern = "_1/shock_soc_080", replacement = "Social shocks")  
shock$shock <- gsub(shock$shock, pattern = "_1/shock_econ_081", replacement = "Economic shocks") 
shock$shock <- gsub(shock$shock, pattern = "_1/shock_env_082", replacement = "Environmental shocks")
  
strat <- data %>% 
  select(contains("strat"), commodity) %>% 
  group_by(commodity) %>% 
  na.omit()

strat <- strat %>% 
  gather(., key="strat", value="value", -commodity)
strat$strat <- gsub(strat$strat, pattern = "_1/strat_migr_083", replacement = "Migration") 
strat$strat <- gsub(strat$strat, pattern = "_1/strat_aid_084", replacement = "External aid")  
strat$strat <- gsub(strat$strat, pattern = "_1/strat_inc_085", replacement = "Looking for new sources of income") 
strat$strat <- gsub(strat$strat, pattern = "_1/strat_exp_086", replacement = "Reducing my household expenses")  
strat$strat <- gsub(strat$strat, pattern = "_1/strat_sav_087", replacement = "Using savings to make up for the shortfall")

library(hrbrthemes)
strategy <- ggplot(data=strat, aes(x=value, fill=commodity)) +
  geom_density(adjust=1.5) +
  facet_wrap(~strat) +scale_fill_manual(values=sample(colors))

ggplotly(strategy)
