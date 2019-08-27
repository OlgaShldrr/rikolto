data <- read_csv("data/data.csv")

training <- data %>% 
  select(contains("145"), `_0/farmer_org_014`) 
training <- training %>% 
  transmute(farmer_org = as.factor(`_0/farmer_org_014`), gender = as.factor(`_5/training_145`))
training <- training %>% 
  group_by(farmer_org, gender) %>% 
  summarise(count = n()) %>% 
  na.omit()
training_plot <- ggplot(training, aes(fill=gender, y=count, x=farmer_org)) + 
  geom_bar(position="dodge", stat="identity")

ggplotly(training_plot)

opinion <- data %>% 
  select(contains("140"), commodity, `_0/sex_012`, `_0/age_011`) 
opinion <- opinion %>% 
  group_by(`_0/sex_012`, `_0/age_011`,`_5/opinion_140`) %>% 
  summarise(count=n()) %>%
  na.omit() %>%
  mutate(fraction = count / sum(count))
  

opinion_plot <- ggplot(opinion, aes(x="", y=fraction, fill=`_5/opinion_140`)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  facet_grid(`_0/age_011`~`_0/sex_012`)

decision <- data %>% 
  select(contains("decision"), `_0/sex_012`,`_0/farmer_org_014`) %>% 
  group_by(`_0/farmer_org_014`, `_0/sex_012`) %>% 
  summarise(average=mean(`_5/decision_146`)) %>% 
  na.omit()
decision$`_0/sex_012` <- as.factor(decision$`_0/sex_012`)
decisions_plot <- ggplot(decision, aes(fill=`_0/sex_012`,y=average, x=`_0/farmer_org_014`)) + 
  geom_bar(position="dodge", stat="identity")
ggplotly(decisions_plot)
