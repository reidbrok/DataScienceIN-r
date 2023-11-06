library(SimDesign)

Design <- expand.grid(sample_size = c (50, 100, 500),
                      pz = c(0.2, 0.8),
                      alpha0 = c(-1, 0, 1),
                      alpha1 = c(0,0.5,1),
                      beta0 = c(-3),
                      beta1 = c(0),
                      beta2 = c(2))

Generate <- function (condition, fixed_objects = NULL ){
  n = condition$sample_size
  pz =  condition$pz
  alpha0 = condition$alpha0
  alpha1 = condition$alpha1
  beta0 = condition$beta0
  beta1 = condition$beta1
  beta2 = condition$beta2
  ## generate confounder Z from a binomial distribution
  z <- rbinom(n, size = 1, prob = pz)
  ## compute probability of observing X = 1 from the inverse logit function
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  ## randomly generate binary variable X from the above probability
  x <- rbinom(n, size = 1, prob = px)
  ## repeat above to randomly generate binary variable Y
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  ## combine three random variables into a data frame
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  dat
}

Analyse <- function ( condition , dat , fixed_objects = NULL ){
  unadj.mod = glm(lung ~ coffee, data = dat, family = "binomial")
  ## fit adjusted logistic regression model
  adj.mod = glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  unadj.coef = summary(unadj.mod)$coef
  adj.coef = summary(adj.mod)$coef
  unadj.p = unadj.coef[2,4]
  adj.p = adj.coef[2,4]
  ret = c(unadjust = unadj.p, adjust = adj.p)
  ret
}
Summarise <- function ( condition , results , fixed_objects = NULL ){
  ret <- EDR(results, alpha = .05 )
  ret
}


results <- runSimulation ( design = Design,
                           replications = 1000 ,
                           parallel = TRUE ,
                           generate = Generate,
                           analyse = Analyse,
                           summarise = Summarise)
library(ggplot2)
library(tidyverse)
final = results %>% pivot_longer(cols = c(unadjust, adjust), values_to = "p.value", names_to = "type")
final$Facetalpha0 <- factor(final$alpha0, levels = c("-1", "0", "1"),
                                             ordered = TRUE, labels=c("alpha[0] == -1", "alpha[0] == 0", "alpha[0] == 1"))
final$Facetsample_size <- factor(final$sample_size, levels = c("50","100", "500"),
                                 ordered = TRUE, labels=c("n == 50", "n == 100", "n == 500"))
ggplot(final, aes (x= alpha1, y= `p.value`, col = type, shape = as.factor(pz))) +
  facet_grid(Facetsample_size~Facetalpha0,labeller = label_parsed) +
  geom_line() + geom_point() + ylim ( c( 0 , 0.6 )) + geom_hline(yintercept = 0.5, color = "grey") + 
  labs(title=expression("Empirical type I error rates for adjusted and unadjusted models
from each simulation scenario (gray dashed horizontal line at 0.05)"), 
       x = bquote(alpha[1]~values), y = "Type I error",shape="Pr(Z = 1)", colour="Model") + theme_minimal()

