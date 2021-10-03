# main
source("R/functions.R")
source("R/models.R")

library(tidyverse)
library(runjags)
library(rjags)


# BM - 1 test non-differential classification -----------------------------
d <- sim_ve_1_imperfect_test(covariates = F,
                            n = 1000,
                            base_dis_prev = 0.1,
                            Se = 0.75,
                            Sp = 0.90,
                            true_OR = 0.2)

d$`estimated OR`
df <- d$data
a <- as.numeric(table(df[df$vax==0,]$dis)); a
b <- as.numeric(table(df[df$vax==1,]$dis)); b
y <- rbind(a, b) %>% 
  as.data.frame %>%
  select(V2, V1) %>%
  as.matrix()
colnames(y) <- c("T+","T-")
rownames(y) <- c("V+","V-")

m = 2
n = apply(y, 1, sum)

## initial values
inits1 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 100022)
inits2 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 300022)
inits3 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 500022)

## run
results <- run.jags(
  bm_1test_nondif,
  n.chains = 3,
  inits = list(inits1, inits2, inits3),
  burnin = 1000,
  sample = 10000
)

print(results)


# BLCM - 2 tests non-differential miss-classification ---------------------

d <- sim_ve_imperfect_tests(covariates = F,
                            n = 1000,
                            base_dis_prev = 0.1,
                            Se1 = 0.95,
                            Sp1 = 0.9,
                            true_OR = 0.2)
y <- data_prep(data=d$data); y
m = 2
n = apply(y, 1, sum)

## run
results <- run.jags(
  blcm_2test_nondif,
  n.chains = 3,
  inits = list(inits1, inits2, inits3),
  burnin = 1000,
  sample = 10000
)

print(results)

plot(
  results,
  vars = list("OR"),
  plot.type = c("trace", "histogram", "autocorr", "ecdf")
)


# BLCM - 2 tests differential classification ------------------------------

d <- sim_ve_imperfect_tests_diff(
  covariates = F,
  n = 1000000,
  base_dis_prev = 0.1,
  Se1_nV = 0.95, # Imperfect test 1 in non-vaccinated
  Sp1_nV = 0.9,
  true_OR = 0.2
)

y <- data_prep(data=d$data); y
m = 2
n = apply(y, 1, sum)

# initial values
inits1 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 100022)
inits2 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 300022)
inits3 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 500022)

## run
results <- run.jags(
  blcm_2test_dif,
  n.chains = 3,
  inits = list(inits1, inits2, inits3),
  burnin =  10000,
  sample = 100000
)

print(results)

plot(
  results,
  vars = list("Se_nV"),
  plot.type = c("trace", "histogram", "autocorr", "ecdf")
)

