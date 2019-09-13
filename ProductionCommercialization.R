

data <- read_csv("data/data.csv")

productivity_table <- data %>% 
  select(productivity, commodity, `farmer organisation`) %>% 
  na.omit() %>% 
  group_by(commodity, `farmer organisation`) %>% 
  summarise(`Average Productivity` = round(mean(productivity), digits=2))

productivity <- data %>% 
  select(productivity, commodity, `farmer organisation`)

library(plotly)

p <- ggplot(na.omit(productivity), aes(x=productivity)) + geom_histogram(binwidth=2,colour="white", fill=colors[3])
p <- p+facet_grid(~commodity)+ labs(x="Productivity, ton", y="Count of farmers by productivity")
ggplotly(p)


production <- data %>% 
  select(production, commodity, `farmer organisation`) %>% 
  na.omit() %>% 
  group_by(commodity, `farmer organisation`) %>% 
  summarise(`Average production` = mean(production))

l<-ggplot(production, aes(x=`farmer organisation`, y=`Average production`)) + geom_bar(stat="identity", fill=colors[1])+
  labs(title = "Production by organization and by commodity", x = "Farmer organization", y = "Average production")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
library(plotly)
ggplotly(l)



library(ggridges)
library(plotly)
farmland_plot <- ggplot(data, aes(x = farmland, y = commodity, fill = commodity)) +
  geom_density_ridges() +
  theme_ridges()+
  theme(legend.position = "none")+
  labs(x="Farmland, ha", y="Number of reported farmland sizes by commodity")+
  scale_x_continuous()+
  scale_fill_manual(values=sample(colors))


commercialization_plot <- ggplot(data, aes(x=sold, y=production, color=commodity)) + 
  geom_point(alpha=0.2)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ylim(0,96)+
  scale_fill_manual(values=sample(colors))+
  labs(x="Tonnes of commodity sold", y="Tonnes of commodity Produced")
ggplotly(commercialization_plot)
