library(tidyverse)
library(here)

disaseter_data_raw <- read.csv(here("original","disaster.csv"))

disaster_data_cleaned = disaseter_data_raw %>% 
  filter(Year <= 2019 & Year >= 2000 & Disaster.Type %in% c("Earthquake","Drought")) %>%
  select(c("Year", "ISO", "Disaster.Type"))%>%
  mutate(Drought = ifelse(Disaster.Type == "Drought",1,0),
         Earthquake = ifelse(Disaster.Type == "Earthquake",1,0)) %>%
  group_by(Year, ISO) %>%
  summarize(Drought = max(Drought), Earthquake = max(Earthquake)) %>%
  mutate(Earthquake = ifelse(is.na(Earthquake)|Earthquake ==0,0,1), Drought = ifelse(is.na(Drought)|Drought ==0,0,1))


write.csv(disaster_data_cleaned, here("data","disaster.csv"))
