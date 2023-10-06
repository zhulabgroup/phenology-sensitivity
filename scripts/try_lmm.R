source("scripts/function_get_clean_npn_first.R")

acer <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/Acer.rds") %>% 
  get_clean_npn_first() 

quercus <- read_rds("/nfs/turbo/seas-zhukai/phenology/NPN/individual_phenometrics/leaf_flower/Quercus.rds") %>% 
  get_clean_npn_first() 

# try a mixed model
acer_stat <- acer %>% 
  group_by(species_id) %>% 
  summarise(observations = n())

acer_3 <- acer %>% 
  filter(species_id == 3) %>% 
  select(first_yes_doy.x,first_yes_doy.y,individual_id)  %>%
  rename(leaf = first_yes_doy.x, flower = first_yes_doy.y)

individualinfo <- acer_3 %>% 
  group_by(individual_id) %>% 
  summarise(indi_count = n())
  
library(lme4)

# Fit the mixed-effects linear model
acer_model <- lmer(flower ~ leaf + (1 | individual_id), data = acer_3)

ggplot(acer_3, aes(x = leaf, y = flower, group = factor(individual_id))) + 
  geom_point(aes(color = factor(individual_id)), alpha = 0.5) + 
  theme_minimal() +
  labs(title = "Raw Data: Flower vs Leaf", x = "Leaf", y = "Flower") +
  theme(legend.position = "none")  # No legend

acer_3$fitted <- predict(acer_model, re.form = NA) # re.form = NA to get fixed-effect predictions

ggplot(acer_3, aes(x = leaf)) +
  geom_point(aes(y = flower, color = factor(individual_id)), alpha = 0.5) +  # raw data
  geom_line(aes(y = fitted, group = individual_id), color = "black") + # model predictions
  theme_minimal() +
  labs(title = "Model Predictions vs Raw Data", x = "Leaf", y = "Flower") +
  theme(legend.position = "none")  # No legend


