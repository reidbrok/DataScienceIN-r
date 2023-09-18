library(tidyverse)
library(here)
library("usethis")

raw_data <- read.csv(here("original","disaster.csv"))

clean_data = raw_data %>% 
  filter(Year <= 2019 & Year >= 2000 & Disaster.Type %in% c("Earthquake","Drought")) %>%
  select(c("Year", "ISO", "Disaster.Type"))%>%
  mutate(Drought = ifelse(Disaster.Type == "Drought",1,0),
         Earthquake = ifelse(Disaster.Type == "Earthquake",1,0)) %>%
  group_by(Year, ISO) %>%
  summarize(Drought = max(Drought), Earthquake = max(Earthquake))

use_git_config(user.name = "reidbrok",user.email = "yushuzou@gmail.com")
use_git()
