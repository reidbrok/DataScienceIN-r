library(tidyverse)
library(here)
library("usethis")

rawdat <- read.csv(here("original","maternalmortality.csv"))

clean_data <- rawdat %>% select(c("Country.Name", "X2000", "X2019")) %>% 
  pivot_longer(c("X2000", "X2019"), names_to = "Year",names_prefix = "X", values_to = "MatMor") %>%
  mutate(Year = as.numeric(Year))

write.csv(clean_data, here("data","clean_data.csv"))


use_git_config(user.name = "reidbrok", user.email = "yushuzou@gmail.com")
use_git()
use_github()
