library(tidyverse)
library(here)
library("usethis")

conflict_data_raw <- read.csv(here("original","conflictdata.csv"))

#### Create binary variable conflict inside of conflict dataframe

#### First, the most commonly employed specification of conflict in the literature is a binary variable
#### indicating the presence of conflict for each country–year observation 
####(0 = no, <25 battle-related deaths; 1 = yes, >=25 battle-related deaths) [29].

conflict = conflict_data_raw %>% group_by(year,ISO) %>% summarize(conflict_country_year = sum(best)) %>% mutate(`armed conflict` = ifelse(conflict_country_year >= 25, 1, 0))
conflict = conflict %>% select(-conflict_country_year)
