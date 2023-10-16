#### Loading Library
library(tidyverse)
library(here)
library(table1)
library(kableExtra)
#### Reading Data
data <- read.csv(here("data","final_analytical_data.csv"))
data <- data %>% filter(Year == 2000) ## Create baseline data
data$armed.conflict <- as.factor(data$armed.conflict)
data$Drought <- as.factor(data$Drought)
data$Earthquake<- as.factor(data$Earthquake)
data$OECD<- as.factor(data$OECD)
# 
# [1] "country_name"            "ISO"                     "region"                 
# [4] "Year"                    "GDP"                     "OECD"                   
# [7] "OECD2023"                "popdens"                 "urban"                  
# [10] "agedep"                  "male_edu"                "temp"                   
# [13] "Drought"                 "Earthquake"              "Infant.mortality.rate"  
# [16] "Under.5.mortality.rate"  "Neonatal.mortality.rate" "Maternal.mortality.rate"
# [19] "armed.conflict" 
### Table 1 with column as predictors; row as outcome
### For the total column: option 1. show baseline characterstic; option 2. show country-year observation
### choose option 1
#my.render.cont <- function(x) {
#  with(stats.apply.rounding(stats.default(x), digits=2), c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
#}
# my.render.cat <- function(x) {
#   c("", sapply(stats.default(x), function(y) with(y,sprintf("%d (%0.0f %%)", FREQ, PCT))))
# }
# 
# render.median.IQR <- function(x, ...) {
#   c('', 
#     `Mean (SD)` = sprintf("%s (&plusmn; %s)", round(mean(x), 2), round(sd(x), 2)),
#     `Median [IQR]` = sprintf("%s [%s, %s]", median(x), 
#                              quantile(x, 0.25), quantile(x, 0.75)))
# }

rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(render.categorical.default(x))
  what <- switch(name,
                 Neonatal.mortality.rate = "Median [Min,Max]",
                 Infant.mortality.rate = "Median [Min,Max]",
                 Under.5.mortality.rate = "Median [Min,Max]",
                 Maternal.mortality.rate = "Median [Min,Max]",
                 GDP = "Mean (SD)",
                 popdens = "Mean (SD)",
                 urban = "Mean (SD)",
                 agedep = "Mean (SD)",
                 male_edu = "Mean (SD)",
                 temp = "Mean (SD)"
                 )
  parse.abbrev.render.code(c("", what))(x)
}



levels(data$armed.conflict) <- c("No","Yes")
labels <- list(
  variables = list(
  GDP = "GDP per capita",
  OECD = "OECD member" ,
  popdens = "Population density",
  urban = "Urban residence",
  agedep = "Age dependency ratio",
  male_edu = "Male education",
  temp = "Temperature",
  Earthquake = "Earthquakes",
  Drought = "Droughts",
  Neonatal.mortality.rate= "Neonatal mortality rate per 1,000 live births",
  Infant.mortality.rate = "Infant mortality rate per 1,000 live births",
  Under.5.mortality.rate = "Under-5 mortality rate per 1,000 live births",
  Maternal.mortality.rate = "Maternal mortality ratio per 100,000 live births"),
  groups=list("","Armed Conflicted"))

# label(data$GDP) = "GDP per capita"
# label(data$OECD) = "OECD member" 
# label(data$popdens) = "Population density"
# label(data$urban) = "Urban residence"
# label(data$agedep) = "Age dependency ratio"
# label(data$male_edu) = "Male education"
# label(data$temp) = "Temperature"
# label(data$Earthquake) = "Earthquakes"
# label(data$Drought) = "Droughts"
# label(data$Neonatal.mortality.rate)= "Neonatal mortality rate per 1,000 live births"
# label(data$Infant.mortality.rate) = "Infant mortality rate per 1,000 live births"
# label(data$Under.5.mortality.rate) = "Under-5 mortality rate per 1,000 live births"
# label(data$Maternal.mortality.rate) = "Maternal mortality ratio per 100,000 live births"

strata <- c(list(Total=data), split(data, data$armed.conflict))

# TABLE <- table1(~ GDP+OECD+popdens+urban+agedep+male_edu+temp+Drought+Earthquake+Infant.mortality.rate+Under.5.mortality.rate+Neonatal.mortality.rate+Maternal.mortality.rate|armed.conflict, 
#        data=data,overall=c(left="Total"),caption = "Table 1. Demographic Table",
#      render = rndr)

#t1kable(TABLE) %>% add_header_above(c(" "," ", "Armed Conflict" = 2))
# 
table1(strata,labels, groupspan=c(1, 2),
       render = rndr,caption = "Table 1. Demographic Table")












