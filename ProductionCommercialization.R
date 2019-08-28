

data <- read_csv("data/data.csv")

productivity_table <- data %>% 
  select(productivity, commodity, `_0/farmer_org_014`) %>% 
  na.omit() %>% 
  group_by(commodity, `_0/farmer_org_014`) %>% 
  summarise(`Average Productivity` = round(mean(productivity), digits=2))

productivity <- data %>% 
  select(productivity, commodity, `_0/farmer_org_014`)

library(plotly)

p <- ggplot(na.omit(productivity), aes(x=productivity)) + geom_histogram(binwidth=2,colour="white", fill=colors[3])
p <- p+facet_grid(~commodity)
ggplotly(p)


production <- data %>% 
  select(production, commodity, `_0/farmer_org_014`) %>% 
  na.omit() %>% 
  group_by(commodity, `_0/farmer_org_014`) %>% 
  summarise(`Average production` = mean(production))

l<-ggplot(production, aes(x=`_0/farmer_org_014`, y=`Average production`, fill=`_0/farmer_org_014`)) + geom_bar(stat="identity", fill=colors[1])+
  labs(title = "Production by organization and by commodity", x = "Farmer organization", y = "Average production")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
library(plotly)
ggplotly(l)



library(ggridges)
library(plotly)
farmland_plot <- ggplot(data, aes(x = farmland, y = commodity, fill = commodity)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")+scale_fill_manual(values=sample(colors))


commercialization_plot <- ggplot(data, aes(x=sold, y=production, color=commodity, size=share_commercialized)) + 
  geom_point(alpha=0.2)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ylim(0,96)+scale_fill_manual(values=sample(colors))
ggplotly(commercialization_plot)
