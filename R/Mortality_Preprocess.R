library(tidyverse)
library(here)
library("usethis")
library(countrycode)

### Read in dataset
rawdat_maternal <- read.csv(here("original","maternalmortality.csv"))
rawdat_infant <- read.csv(here("original","infantmortality.csv"))
rawdat_neonatal <- read.csv(here("original","neonatalmortality.csv"))
rawdat_under5 <- read.csv(here("original","under5mortality.csv"))

### Clean Dataset
DataClean <- function(x){
  x%>% select(c("Country.Name", "X2000":"X2019")) %>% 
    pivot_longer(c("X2000":"X2019"), names_to = "Year",names_prefix = "X", values_to = "MatMor") %>%
    mutate(Year = as.numeric(Year))
}

clean_maternal <- DataClean(rawdat_maternal)
clean_infant <- DataClean(rawdat_infant)
clean_neonatal <- DataClean(rawdat_neonatal)
clean_under5 <-  DataClean(rawdat_under5)


list_morality <- list(clean_infant, clean_under5, clean_neonatal,clean_maternal)
for(i in 1:4){
  list_morality[[i]]$morality <- i
}
#### Generate dataframe
data_morality <- Reduce(full_join,list_morality)

#### Modify the dataset
data_morality$morality = case_when(data_morality$morality == 1 ~ "Infant mortality rate",
                                   data_morality$morality == 2 ~ "Under 5 mortality rate",
                                   data_morality$morality == 3 ~ "Neonatal mortality rate",
                                   data_morality$morality == 4 ~ "Maternal mortality rate"
)

data_morality_wide = data_morality %>%
  pivot_wider(names_from = morality, values_from = MatMor)

### Change the country name to country code

data_morality_wide$ISO = countrycode(data_morality_wide$Country.Name, origin = 'country.name', destination = 'iso3c') 
data_morality_wide = data_morality_wide %>% select(-Country.Name)
data_morality_wide = data_morality_wide[, c(6, 1, 2, 3, 4,5)]
write.csv(data_morality_wide, here("data","cleaned_data_morality.csv"))
