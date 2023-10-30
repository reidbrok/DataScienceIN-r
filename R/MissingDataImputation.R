#### Library
library(here)
library(tidyverse)
library(mice)
library(VIM)
library(scales)
library(texreg)
#### Read in final data, which contains the missing data
data <-  read.csv(here("data","finaldata.csv"))

#### GDP was “scaled up by 1,000” and population density was rescaled from 0 to 1.
data$GDP = data$GDP/1000
data$popdens = rescale(data$popdens)
data$ISO
#### Missing Data Visualization
VIM::aggr(data, numbers = TRUE, prop = c(TRUE, FALSE))

summary(data)
#### prepare for the linear regression
preds <- as.formula("~ armconf1 + GDP + OECD + popdens + urban + agedep + male_edu + temp + drought + earthquake + ISO +as.factor(year)")

matmormod <- lm(update.formula(preds, matmor ~ .), data = data)
un5mormod <- lm(update.formula(preds, un5mor ~ .), data = data)       
neomormod <- lm(update.formula(preds, neomor ~ .), data = data) 
infmormod <- lm(update.formula(preds, infmor ~ .), data = data) 
keepvars <- list("armconf1" = "Armed conflict",
                 "GDP1000" = "GDP",
                 "OECD" = "OECD",
                 "popdens100" = "Population density",
                 "urban" = "Urban",
                 "AgeDep" = "Age dependency",
                 "male_edu" = "Male education",
                 "temp" = "Average temperature",
                 "rainfall" = "Average rainfall",
                 "earthquake" = "Earthquake",
                 "drought" = "Drought")
texreg(list(matmormod, un5mormod, infmormod, neomormod),
       ci.force = TRUE,
       custom.coef.map = keepvars,
       custom.model.names = c("Maternal mortality", "Under-5 mortality",
                              "Infant mortality", "Neonatal mortality"),
       caption = "Results from linear regression models")

########## Multiple imputation
#### Data Wrangling
data$ISO = as.numeric(as.factor(data$ISO))
mi0 <- mice(data, seed = 1, m = 1, maxit = 0, print = F)
#### Level 1 data 
mi0$method[c( 'GDP', 'popdens', 'urban', 'male_edu','temp',"matmor", 'un5mor', "infmor", "neomor" )] <- "2l.pan"
mi0$predictorMatrix[c('GDP', 'popdens', 'urban', 'male_edu','temp',"matmor", 'un5mor', "infmor", "neomor" ), "ISO"] <- -2
mice.multi.out  <- mice(data, seed = 1, m = 10, maxit = 20,
                        method = mi0$method,
                        predictorMatrix = mi0$predictorMatrix, print = F)
save(mice.multi.out, file = "miceout.Rda")
load("miceout.Rda")
plot(mice.multi.out)
complete.data.multi2 <- complete(mice.multi.out, "all")

#### Rerun the model
matmormod_mice <- lm(update.formula(preds, matmor ~ .), data = complete.data.multi2$`1`)
un5mormod_mice <- lm(update.formula(preds, un5mor ~ .), data = complete.data.multi2$`1`)       
neomormod_mice <- lm(update.formula(preds, neomor ~ .), data = complete.data.multi2$`1`) 
infmormod_mice <- lm(update.formula(preds, infmor ~ .), data = complete.data.multi2$`1`) 
keepvars <- list("armconf1" = "Armed conflict",
                 "GDP1000" = "GDP",
                 "OECD" = "OECD",
                 "popdens100" = "Population density",
                 "urban" = "Urban",
                 "AgeDep" = "Age dependency",
                 "male_edu" = "Male education",
                 "temp" = "Average temperature",
                 "rainfall" = "Average rainfall",
                 "earthquake" = "Earthquake",
                 "drought" = "Drought")

texreg(list(matmormod, un5mormod, infmormod, neomormod,matmormod_mice, un5mormod_mice, infmormod_mice, neomormod_mice),
       ci.force = TRUE,custom.header = list("Complete Case" = 1:4, "Multiple imputation" = 5:8),
       custom.coef.map = keepvars,
       custom.model.names = c("Maternal mortality", "Under-5 mortality",
                              "Infant mortality", "Neonatal mortality",
                              "Maternal mortality", "Under-5 mortality",
                              "Infant mortality", "Neonatal mortality"),
       caption = "Results from linear regression models")
