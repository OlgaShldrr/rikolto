#productivity <- data %>% 
#  select(contains("productivity"), -contains("OUTLIER"), -contains("DISPLAY"))
#productivity$x <- paste(productivity$`_3/crop_productivity_105_rice`,productivity$`_3/crop_productivity_105_ses`, productivity$`_3/crop_productivity_105_pul`, 
#           productivity$`_3/crop_productivity_105_coc`,  productivity$`_3/crop_productivity_105_cof`,  productivity$`_3/crop_productivity_105_cin`,
#          productivity$`_3/crop_productivity_105_veg`,  productivity$`_3/crop_productivity_105_gb`,  productivity$`_3/crop_productivity_105_ba`, 
#          productivity$`_3/crop_productivity_105_to`,   productivity$`_3/crop_productivity_105_chi`,  productivity$`_3/crop_productivity_105_ca`,  
#          productivity$`_3/crop_productivity_105_on`,   productivity$`_3/crop_productivity_105_cab`,  productivity$`_3/crop_productivity_105_let` , sep = ", ")
#productivity$x <- gsub(productivity$x, pattern="NA, ", replacement = "")
#productivity$x <- gsub(productivity$x, pattern=", NA", replacement = "")
#productivity$x <- as.numeric(productivity$x)
#data$productivity <- productivity$x
#
#write.csv(data, "data/data.csv", row.names = FALSE)

data <- read_csv("data/data.csv")

productivity_table <- data %>% 
  select(productivity, commodity, `_0/farmer_org_014`) %>% 
  na.omit() %>% 
  group_by(commodity, `_0/farmer_org_014`) %>% 
  summarise(`Average Productivity` = round(mean(productivity), digits=2))

productivity <- data %>% 
  select(productivity, commodity, `_0/farmer_org_014`)

library(plotly)

p <- ggplot(na.omit(productivity), aes(x=productivity)) + geom_histogram(binwidth=2,colour="white")
p <- p+facet_grid(~commodity)
ggplotly(p)

production <- data %>% 
  select(contains("production"), -contains("OUTLIER"), -contains("invest"))
production$x<-paste(production$`_3/crop_production_103_rice`, production$`_3/crop_production_103_ses`, production$`_3/crop_production_103_pul`, production$`_3/crop_production_103_coc`, 
production$`_3/crop_production_103_cof`, production$`_3/crop_production_103_cin`, production$`_3/crop_production_103_veg`, production$`_3/crop_production_103_gb`,
production$`_3/crop_production_103_ba`, production$`_3/crop_production_103_to`, production$`_3/crop_production_103_chi`, production$`_3/crop_production_103_ca`, 
    production$`_3/crop_production_103_on`, production$`_3/crop_production_103_cab`, production$`_3/crop_production_103_let`, sep=", ")
production$x <- gsub(production$x, pattern="NA, ", replacement = "")
production$x <- gsub(production$x, pattern=", NA", replacement = "")
production$x <- as.numeric(production$x)
data$production <- production$x

write.csv(data, "data/data.csv", row.names = FALSE)

production <- data %>% 
  select(production, commodity, `_0/farmer_org_014`) %>% 
  na.omit() %>% 
  group_by(commodity, `_0/farmer_org_014`) %>% 
  summarise(`Average production` = mean(production))

l<-ggplot(production, aes(x=`_0/farmer_org_014`, y=`Average production`)) + geom_bar(stat="identity")+
  labs(title = "Production by organization and by commodity", x = "Farmer organization", y = "Average production")
library(plotly)
ggplotly(l)

farmland <- data %>%  select(contains("dedicated_farmland"), -contains("OUTLIER"))
farmland$x <- paste(farmland$`_3/dedicated_farmland_102_rice`,
  farmland$`_3/dedicated_farmland_102_ses`,
  farmland$`_3/dedicated_farmland_102_pul`,
  farmland$`_3/dedicated_farmland_102_coc`,
  farmland$`_3/dedicated_farmland_102_cof`,
  farmland$`_3/dedicated_farmland_102_cin`,
  farmland$`_3/dedicated_farmland_102_veg`,
  farmland$`_3/dedicated_farmland_102_gb`,
  farmland$`_3/dedicated_farmland_102_ba` ,
  farmland$`_3/dedicated_farmland_102_to`,
  farmland$`_3/dedicated_farmland_102_chi`,
  farmland$`_3/dedicated_farmland_102_ca`,
  farmland$`_3/dedicated_farmland_102_on` ,
  farmland$`_3/dedicated_farmland_102_cab`,
  farmland$`_3/dedicated_farmland_102_let`, sep=", ")
farmland$x <- gsub(farmland$x, pattern="NA, ", replacement = "")
farmland$x <- gsub(farmland$x, pattern=", NA", replacement = "")
farmland$x <- as.numeric(farmland$x)
data$farmland <- farmland$x

library(ggridges)
library(plotly)
farmland_plot <- ggplot(data, aes(x = farmland, y = commodity, fill = commodity)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


sold <- data %>% 
  select(contains("107"), -contains("OUTLIER")) %>% 
  transmute(sold_107_rice = data$`_3/sold_formal_fo_107_rice`+data$`_3/sold_formal_com_107_rice`,
         sold_107_coffee = data$`_3/sold_formal_fo_107_cof`,
        sold_107_cocoa= data$`_3/sold_formal_fo_107_coc`,
         sold_107_cinnamon=data$`_3/sold_formal_fo_107_cin`)

sold$x <- paste(sold$sold_107_rice, sold$sold_107_coffee, sold$sold_107_cocoa, sold$sold_107_cinnamon, sep=", ")
sold$x <- gsub(sold$x, pattern="NA, ", replacement = "")
sold$x <- gsub(sold$x, pattern=", NA", replacement = "")
sold$x <- as.numeric(sold$x)
data$sold <- sold$x

data$share_commercialized <- data$sold/data$production
data$commodity <- as.factor(data$commodity)
write.csv(data, "data/data.csv", row.names = FALSE)


commercialization_plot <- ggplot(data, aes(x=sold, y=production, color=commodity, size=share_commercialized)) + 
  geom_point(alpha=0.2)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ylim(0,96)
ggplotly(commercialization_plot)
