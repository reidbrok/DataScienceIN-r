library(here)
library(tidyverse)

source(here("R","Mortality_Preprocess.R"))
source(here("R","ArmedConflict_CreateVariable.R"))

source(here("R","Disaster_Preprocess.R"))
covariate = read.csv(here("original","covariates.csv"))

# Filter the covariate
covariate = covariate %>% filter(year <= 2019 & year >= 2000)
# Check the ISO in disaster data
disaster_data_cleaned %>% group_by(ISO) %>% summarise(n())

# Rename the share column
colnames(covariate)[4] = "Year"
colnames(conflict)[1] = "Year"

# Merge Data
dataframe_list = list(covariate,disaster_data_cleaned,data_morality_wide,conflict)
final_data = Reduce(left_join,dataframe_list)

# Output Final Data
write.csv(final_data, here("data","final_analytical_data.csv"),row.names=FALSE)

          