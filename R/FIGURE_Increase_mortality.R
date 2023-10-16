#### Loading Library
library(tidyverse)
library(here)
library(ggplot2)
#### Reading Data
data <- read.csv(here("data","final_analytical_data.csv"))

### Find maternal mortality of 2017 and 2000
data_2000 <- data %>% filter(Year == 2000) %>% select("Year","ISO","Maternal.mortality.rate")

data_2017 <- data %>% filter(Year == 2017) %>% select("Year","ISO","Maternal.mortality.rate")

### compare the maternal mortality on 2017 and 2000, and find countries that have higher maternal mortality on 2017 than 2000
merge_data <- merge(data_2000, data_2017, by = "ISO") %>% filter(Maternal.mortality.rate.y > Maternal.mortality.rate.x) %>% select(ISO)

### find these countries in the complete data
monotone_data <- data %>% filter(ISO %in% merge_data$ISO)
monotone_data %>% 
  ggplot(aes(x=Year, y = Maternal.mortality.rate, color = ISO)) + 
  geom_line(aes(group=ISO)) +
  facet_wrap(~armed.conflict,labeller = labeller(armed.conflict = new_labels))+
  scale_y_log10() + theme_minimal()+
  labs(
    x = "Year",
    y = "Maternal Mortality Rate"
  )
