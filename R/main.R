# main
source("R/functions.R")
source("R/blcm_models.R")

library(tidyverse)
library(runjags)
library(rjags)


# BLCM - 2 tests non-differential classification --------------------------

d <- sim_ve_imperfect_tests(covariates = F,
                            n = 1000,
                            base_dis_prev = 0.1,
                            Se1 = 0.95,
                            Sp1 = 0.9,
                            true_OR = 0.2)
y <- data_prep(data=d$data); y
m = 2
n = apply(y, 1, sum)

## initial values
inits1 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 100022)
inits2 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 300022)
inits3 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 500022)

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

