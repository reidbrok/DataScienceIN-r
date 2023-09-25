library(here)
library(tidyverse)

source(here("R","clean_data.R"))
source(here("R","create_armed_conflict.R"))

source(here("R","clean_data_disaster.R"))
covariate = read.csv(here("original","covariates.csv"))
colnames(covariate)[4] = "Year"
colnames(conflict)[1] = "Year"
dataframe_list = list(covariate,disaster_data_cleaned,data_morality_wide,conflict)
final_data = Reduce(full_join,dataframe_list)
write.csv(final_data, "final_analytical_data.csv")
