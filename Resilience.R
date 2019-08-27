info <- data %>% 
  select(contains("info"), -contains("informal"), commodity) %>% 
  group_by(commodity) %>% 
  summarise_all(funs(mean)) %>%
  na.omit()

info <- info %>% 
  gather(., key="info", value="value", -commodity)

info_plot <- ggplot(info, aes(fill=commodity, y=value, x=info)) + 
  geom_bar(position="dodge", stat="identity")

ggplotly(info_plot)

shock <- data %>% 
  select(contains("shock"), commodity, `_0/farmer_org_014`) %>% 
  group_by(commodity, `_0/farmer_org_014`) %>%
  summarise_all(funs(mean))

shock <- shock %>% 
  gather(., key="shock", value="value", -commodity, -`_0/farmer_org_014`)

  
strat <- data %>% 
  select(contains("strat"), commodity) %>% 
  group_by(commodity) %>% 
  na.omit()

strat <- strat %>% 
  gather(., key="strat", value="value", -commodity)
library(hrbrthemes)
strategy <- ggplot(data=strat, aes(x=value, fill=commodity)) +
  geom_density(adjust=1.5) +
  facet_wrap(~strat) 

ggplotly(strategy)
