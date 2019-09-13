library(tidyverse)

data <- read_csv("data/data.csv")

drivers <- data %>% 
  select(contains("130"), commodity)
drivers <- drivers %>% 
  gather(., key="drivers", value = "value", -commodity)
drivers$value <- gsub(drivers$value,pattern = "land", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "irrigation", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "weather", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "climate", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "price", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "demand", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "capital", replacement = 1)
drivers$value <- gsub(drivers$value,pattern = "labour", replacement = 1)
drivers <- drivers %>% 
  filter(value==1)
drivers_counts <- drivers %>% 
  filter(!is.na(value)) %>% 
  group_by(commodity, drivers) %>% 
  summarise(Percent=n())
drivers_counts$drivers <- gsub(drivers_counts$drivers, pattern = "_4/prod_planning_130/land", replacement = "Land capacity/conservation measures (need to leave land fallow, soil erosion etc.)")      
drivers_counts$drivers <- gsub(drivers_counts$drivers, pattern = "_4/prod_planning_130/labour", replacement = "Labour availability")    
drivers_counts$drivers <- gsub(drivers_counts$drivers, pattern = "_4/prod_planning_130/capital", replacement = "Capital availability (financial capital, but also other inputs)")   
drivers_counts$drivers <- gsub(drivers_counts$drivers, pattern = "_4/prod_planning_130/demand", replacement = "Demand from FO") 
drivers_counts$drivers <- gsub(drivers_counts$drivers, pattern = "_4/prod_planning_130/price", replacement = "Market price of different crops/commodities")     
drivers_counts$drivers <- gsub(drivers_counts$drivers, pattern = "_4/prod_planning_130/weather", replacement = "Weather forecast")  
drivers_counts$drivers <- gsub(drivers_counts$drivers, pattern = "_4/prod_planning_130/climate", replacement = "Climate variability")   
drivers_counts$drivers <- gsub(drivers_counts$drivers, pattern = "_4/prod_planning_130/irrigation", replacement = "Irrigation capacity")

drivers_plot <- ggplot(drivers_counts, aes(fill=drivers, y=Percent, x=commodity)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values=sample(colors))+
  theme(legend.position = "none")+
  labs(x="Commodity", y="Percentage of farmers")


ggplotly(drivers_plot)


access <- data %>% 
  select(contains("091"), commodity) 
access <- access %>% 
  gather(., key="access", value = "value", -commodity)
access <- access %>% 
  filter(!is.na(value))
access_counts <- access %>% 
  filter(!is.na(value)) %>% 
  group_by(access) %>% 
  summarise(count=n()) %>% 
  mutate(fraction = count / sum(count))
access_counts <- access_counts %>% 
  mutate(ymax = cumsum(fraction))
access_counts <- access_counts %>% 
  mutate(ymin = c(0, head(ymax, n=-1)))
access_counts$access <- gsub(access_counts$access, pattern = "_1/credit_acc_091/bank", replacement = "Banks")       
access_counts$access <- gsub(access_counts$access, pattern = "_1/credit_acc_091/family", replacement = "Family")     
access_counts$access <- gsub(access_counts$access, pattern = "_1/credit_acc_091/buyer", replacement = "Buyer company")    
access_counts$access <- gsub(access_counts$access, pattern = "_1/credit_acc_091/community", replacement = "Community")   
access_counts$access <- gsub(access_counts$access, pattern = "_1/credit_acc_091/fo", replacement = "Farmer organisation")          
access_counts$access <- gsub(access_counts$access, pattern = "_1/credit_acc_091/intermediary", replacement = "Intermediaries")  
access_counts$access <- gsub(access_counts$access, pattern = "_1/credit_acc_091/mfi", replacement = "Micro Financial Institutions")  

# Compute label position
access_counts$labelPosition <- (access_counts$ymax + access_counts$ymin) / 2

# Compute a good label
access_counts$label <- paste0(round(access_counts$fraction, digits = 2)*100," %")

access_plot <- ggplot(access_counts, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=access)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  theme_void() +
  xlim(c(2, 4)) +scale_fill_manual(values=sample(colors))
#+
#  facet_grid(~ commodity)# Try to remove that to see how to make a pie chart



why_no_acess <- data %>% 
  select(contains("092"), Commodity = commodity) 
why_no_acess <- why_no_acess %>% 
  gather(., key="noaccess", value = "value", -Commodity)
why_no_acess <- why_no_acess %>% 
  filter(!is.na(value)) %>% 
  group_by(Commodity, noaccess) %>% 
  summarise(Count=n()) %>% 
  mutate(`Proportion within the same commodity type, %` = round(Count / sum(Count)*100, digits=2))

why_no_acess$noaccess <- gsub(why_no_acess$noaccess, pattern = "_1/credit_acc_092/collateral", replacement = "I had insufficient collateral")      
why_no_acess$noaccess <- gsub(why_no_acess$noaccess, pattern = "_1/credit_acc_092/interest", replacement = "I could not afford the interest rates offered to me")    
why_no_acess$noaccess <- gsub(why_no_acess$noaccess, pattern = "_1/credit_acc_092/unrealistic", replacement = "My credit/loan request was not realistic in the eyes of the finance institutions")
why_no_acess$noaccess <- gsub(why_no_acess$noaccess, pattern = "_1/credit_acc_092/nomfi", replacement = "There is no finance institution in my area")
why_no_acess$noaccess <- gsub(why_no_acess$noaccess, pattern = "_1/credit_acc_092/failure", replacement = "I had failed to meet the repayment conditions of a previous loan or credit")
why_no_acess$noaccess <- gsub(why_no_acess$noaccess, pattern = "_1/credit_acc_092/repayment", replacement = "I would not be able to meet the repayment deadlines") 

why_no_acess$`Reasons for no access` <- why_no_acess$noaccess
why_no_acess <- why_no_acess %>% 
  select(Commodity, `Reasons for no access`, Count, `Proportion within the same commodity type, %`)
investment <- data %>% 
  select(contains("131"), commodity) 
investment <- investment %>% 
  gather(., key="investment", value = "value", -commodity)
investment_counts <- investment %>% 
  filter(!is.na(value)) %>% 
  group_by(investment) %>% 
  summarise(count=n()) %>% 
  mutate(fraction = count / sum(count))
investment_counts$investment <- gsub(investment_counts$investment, pattern = "_4/invest_131/commercialisation", replacement = "Support of the FO\n for commercialisation")
investment_counts$investment <- gsub(investment_counts$investment, pattern = "_4/invest_131/diversification", replacement = "To diversify my \nincome sources") 
investment_counts$investment <- gsub(investment_counts$investment, pattern = "_4/invest_131/income", replacement = "It is the only way for me\n to generate  an income")           
investment_counts$investment <- gsub(investment_counts$investment, pattern = "_4/invest_131/processing", replacement = "Support of the FO \nfor processing")       
investment_counts$investment <- gsub(investment_counts$investment, pattern = "_4/invest_131/production", replacement = "Support of the FO \nfor production")       
investment_counts$investment <- gsub(investment_counts$investment, pattern = "_4/invest_131/profitability", replacement = "Profitability \nof the crop")    
investment_counts$investment <- gsub(investment_counts$investment, pattern = "_4/invest_131/vulnerability", replacement = "To reduce vulnerability\n from climate change")

k <- ggplot(investment_counts, aes(x=investment, y=count)) +
  geom_segment( aes(x=investment, xend=investment, y=0, yend=count)) +
  labs(x="Motivations for investment", y="Count of farmers")+
  geom_point( size=5, color=colors[2], fill=alpha(colors[1], 0.3), alpha=0.7, shape=21, stroke=2) +scale_fill_manual(values=sample(colors))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(k)
