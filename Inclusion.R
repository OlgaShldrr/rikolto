data <- read_csv("data/data.csv")

training <- data %>% 
  select(contains("145"), `farmer organisation`) 
training <- training %>% 
  transmute(farmer_org = as.factor(`farmer organisation`), `Who represents` = as.factor(`_5/training_145`))
training <- training %>% 
  group_by(farmer_org, `Who represents`) %>% 
  summarise(count = n()) %>% 
  na.omit()
training_plot <- ggplot(training, aes(fill=`Who represents`, y=count, x=farmer_org)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=sample(colors))+
  labs(x="Farmer Organisation", y="Number of farmers giving the answer")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(training_plot)

opinion <- data %>% 
  select(contains("140"), commodity, sex, age) 
opinion$`Is your opinion respected/\n taken into account?` <- opinion$`_5/opinion_140`
opinion$`Is your opinion respected/\n taken into account?` <- gsub(opinion$`Is your opinion respected/\n taken into account?`, pattern=0, replacement = "Less than others")
opinion$`Is your opinion respected/\n taken into account?` <- gsub(opinion$`Is your opinion respected/\n taken into account?`, pattern=1, replacement = "The same as others")
opinion$`Is your opinion respected/\n taken into account?` <- gsub(opinion$`Is your opinion respected/\n taken into account?`, pattern=2, replacement = "More than others")
opinion <- opinion %>% 
  group_by(sex, age,`Is your opinion respected/\n taken into account?`) %>% 
  summarise(count=n()) %>%
  na.omit() %>%
  mutate(fraction = count / sum(count))
  

opinion_plot <- ggplot(opinion, aes(x="", y=fraction, fill=`Is your opinion respected/\n taken into account?`)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
#  geom_label(x=2.5, aes(y=2, label=paste0(round(opinion$fraction, digits=2)*100, " %"), size=6)) +
  theme_void() + 
  theme(margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
  facet_grid(age~sex)+scale_fill_manual(values=sample(colors))


decision <- data %>% 
  select(contains("decision"), sex,`farmer organisation`) %>% 
  group_by(`farmer organisation`, sex) %>% 
  summarise(average=mean(`_5/decision_146`)) %>% 
  na.omit()
decision$sex <- as.factor(decision$sex)
decisions_plot <- ggplot(decision, aes(fill=sex,y=average, x=`farmer organisation`)) + 
  geom_bar(position="dodge", stat="identity")+
  labs(x="Farmer organisation", y="Average degree of control over decision making")+
  scale_fill_manual(values=sample(colors))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(decisions_plot)
