library(boot)
library(here)
library(tidyverse)
library(gridExtra)
set.seed(7001)
data <- read.csv(here("data", "finaldata.csv"))

data2017 <- data%>% 
  dplyr::filter(year == 2017 & !is.na(matmor) & !is.na(neomor) & !is.na(infmor) & !is.na(un5mor))%>%
  dplyr::select(ISO, matmor,neomor,infmor,un5mor,armconf1 )
#### BOOTSTRAPPING
B <- 1000
######  maternal mortality
getmeddiff <- function(data, num, indices) {
  sample_data <- data[indices, ]
  med.mor <- tapply(sample_data[,num], sample_data$armconf1, FUN = median)
  med.mor.diff <- med.mor[2] - med.mor[1]
  return(med.mor)
}

bootout.mor <- boot(data2017, num = 2, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout.neo <- boot(data2017, num = 3, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout.inf <- boot(data2017, num = 4, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout.un5 <- boot(data2017, num = 5, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)



mor_95 = boot.ci(boot.out = bootout.mor, conf = 0.95, type = c("basic", "perc", "bca"))
neo_95 = boot.ci(boot.out = bootout.neo, conf = 0.95, type = c("basic", "perc", "bca"))
inf_95 = boot.ci(boot.out = bootout.inf, conf = 0.95, type = c("basic", "perc", "bca"))
un5_95 = boot.ci(boot.out = bootout.un5, conf = 0.95, type = c("basic", "perc", "bca"))


TABLE <- data.frame( Covariate = c("Maternal", "Neonatal","Infant","Under 5"),
                     Estimate = rep(NA,4))

TABLE$Estimate = unlist(c(mor_95$t0,neo_95$t0, inf_95$t0, un5_95$t0),use.names = F)
TABLE$`Standard Deviation` = unlist(c(round(sd(bootout.mor$t),2),round(sd(bootout.neo$t),2), round(sd(bootout.inf$t),2), round(sd(bootout.un5$t),2)),use.names = F)
TABLE$`2.5% BCA` = unlist(c(mor_95$bca[,4],neo_95$bca[,4], inf_95$bca[,4], un5_95$bca[,4]),use.names = F)
TABLE$`97.5% BCA` = unlist(c(mor_95$bca[,5],neo_95$bca[,5], inf_95$bca[,5], un5_95$bca[,5]),use.names = F)
TABLE$`Sample Size` = rep(nrow(data2017),4)


library(gridExtra)
ggsave("BoostrappingCI.pdf",plot=tableGrob(TABLE), path = here("data"))
