#main
source("R/functions.R")
source("R/blcm_models.R")

library(tidyverse)
library(runjags)
library(rjags)

# Generate data
d <- sim_ve_imperfect_tests(covariates = F,
                            n = 100000,
                            Se1 = 0.95,
                            Sp1 = 0.9,
                            true_OR = 0.75)

df <- d$data
d$`estimated OR T1`
d$`estimated OR T2`
d$`true OR`
t1 <- table(df$vax,df$dis1)
estimated_OR <- (t1[1] * t1[4]) / (t1[2] * t1[3])
estimated_OR # check
t2 <- table(df$vax,df$dis2)
estimated_OR <- (t2[1] * t2[4]) / (t2[2] * t2[3])
estimated_OR # check

# BLCM - 2 tests Non differential classification
a <- as.numeric(table(df[df$vax==0,]$dis1, df[df$vax==0,]$dis2))
b <- as.numeric(table(df[df$vax==1,]$dis1, df[df$vax==1,]$dis2))
c <- table(df$dis1, df$dis2, df$vax);c
a
b
y <- rbind(a, b) %>% 
  as.data.frame %>%
  select(V4, V2, V3, V1) %>%
  as.matrix()

y
m = 2
n = apply(y, 1, sum)

# initial values
inits1 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 100022)
inits2 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 300022)
inits3 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 500022)

# Running
results <- run.jags(blcm_2test_nondif, 
                    n.chains = 3,
                    inits=list(inits1, inits2, inits3),
                    burnin = 10000,
                    sample = 100000)
print(results)
# 
# plot(results,
#      vars = list("OR"),
#      plot.type = c("trace", "histogram", "autocorr"))
