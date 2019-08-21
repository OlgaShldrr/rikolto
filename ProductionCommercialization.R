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

productivity <- data %>% 
  select(productivity, commodity, `_0/farmer_org_014`) %>% 
  na.omit() %>% 
  group_by(commodity, `_0/farmer_org_014`) %>% 
  summarise(`Average Productivity` = mean(productivity))


#production <- data %>% 
#  select(contains("production"), -contains("OUTLIER"), -contains("invest"))
#production$x<-paste(production$`_3/crop_production_103_rice`, production$`_3/crop_production_103_ses`, production$`_3/crop_production_103_pul`, production$`_3/crop_production_103_coc`, 
#production$`_3/crop_production_103_cof`, production$`_3/crop_production_103_cin`, production$`_3/crop_production_103_veg`, production$`_3/crop_production_103_gb`,
#production$`_3/crop_production_103_ba`, production$`_3/crop_production_103_to`, production$`_3/crop_production_103_chi`, production$`_3/crop_production_103_ca`, 
#    production$`_3/crop_production_103_on`, production$`_3/crop_production_103_cab`, production$`_3/crop_production_103_let`, sep=", ")
#production$x <- gsub(production$x, pattern="NA, ", replacement = "")
#production$x <- gsub(production$x, pattern=", NA", replacement = "")
#production$x <- as.numeric(production$x)
#data$production <- production$x
#
#write.csv(data, "data/data.csv", row.names = FALSE)
