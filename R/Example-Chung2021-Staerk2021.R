# load packages -----------------------------------------------------------
list.of.packages <- c("tidyverse", "runjags", "rjags", "rootSolve", "parallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(runjags)
library(rjags)
library(rootSolve)
library(parallel)
testjags()
source("R/functions.R") 


# lecuyer initial values for parallelized JAGS code -----------------------

load.module("lecuyer")
inits1 <- list(.RNG.name = "lecuyer::RngStream", .RNG.seed = 2020)
inits2 <- list(.RNG.name = "lecuyer::RngStream", .RNG.seed = 2021)
inits3 <- list(.RNG.name = "lecuyer::RngStream", .RNG.seed = 2022)


# Predictive prior check --------------------------------------------------
# Sp
HPSp <- findbetaqq2(
  percentile.value1 = 0.9973,
  percentile1 = 0.025,
  percentile.value2 = 0.9997,
  percentile2 = 0.975
)
HPSp
round(qbeta(c(0.025, 0.5, 0.975),
            HPSp[1], HPSp[2]), 4)

HPSp_nV <- HPSp
HPSp_V <- HPSp

# Se
HPSe_nV <- findbetaqq2(
  percentile.value1 = 0.9279,
  percentile1 = 0.025,
  percentile.value2 = 0.9843,
  percentile2 = 0.975
)
HPSe_nV
round(qbeta(c(0.025, 0.5, 0.975), 
            HPSe_nV[1], HPSe_nV[2]), 4)

HPSe_V <- findbetaqq2(
  percentile.value1 = 0.9155,
  percentile1 = 0.025,
  percentile.value2 = 0.9999,
  percentile2 = 0.975
)
HPSe_V
round(qbeta(c(0.025, 0.5, 0.975), # check
            HPSe_V[1], HPSe_V[2]), 4)


bm_1t_dif <- " model {

  # likelihood

  y[1, 1:2] ~ dmulti(prob[1, 1:2], N[1])
  prob[1, 1] <- pi[1] * Se_nV + (1 - pi[1]) * (1 - Sp_nV)
  prob[1, 2] <- pi[1] * (1 - Se_nV) + (1 - pi[1]) * Sp_nV
  
  y[2, 1:2] ~ dmulti(prob[2, 1:2], N[2])
  prob[2, 1] <- pi[2] * Se_V + (1 - pi[2]) * (1 - Sp_V)
  prob[2, 2] <- pi[2] * (1 - Se_V) + (1 - pi[2]) * Sp_V
  
  # prior for proportions
  pi[1] ~ dbeta(1, 1)
  pi[2] ~ dbeta(1, 1)
  
  # priors for se/sp
  Se_V ~ dbeta(HPSe_V[1], HPSe_V[2])
  Se_nV ~ dbeta(HPSe_nV[1], HPSe_nV[2])
  Sp ~ dbeta(HPSp[1], HPSp[2])
  Sp_nV <- Sp
  Sp_V <- Sp
  
  # Computing OR
  OR <- (pi[2] / (1 - pi[2])) / (pi[1] / (1 - pi[1]))
  VE <- (1-OR)*100

  #data# HPSp, HPSe_nV, HPSe_V, y, N
  #inits#
  #monitor# Se_V, Se_nV, Sp, pi, OR, VE
  }
"

# data
y <- matrix(c(51220, 251541,
              57,3817), nrow = 2, byrow = T)
N <- apply(y, 1, sum)

res_hw <- run.jags(
  bm_1t_dif,
  n.chains = 3,
  burnin = 15000,
  sample = 60000,
  inits = list(inits1, inits2, inits3)
)
res_hw

plot(
  res_hw,
  plot.type = c("h", "t", "au"),
  vars = c("OR", "VE", "pi"),
  layout = c(4, 3)
)


# Predictive prior check --------------------------------------------------

bm_1t_prior_check <- " model {

  # prior for proportions
  pi[1] ~ dbeta(1, 1)
  pi[2] ~ dbeta(1, 1)
  
  # priors for se/sp
  Se_V ~ dbeta(HPSe_V[1], HPSe_V[2])
  Se_nV ~ dbeta(HPSe_nV[1], HPSe_nV[2])
  Sp ~ dbeta(HPSp[1], HPSp[2])
  Sp_nV <- Sp
  Sp_V <- Sp
  
  # Computing OR
  OR <- (pi[2] / (1 - pi[2])) / (pi[1] / (1 - pi[1]))
  VE <- (1-OR)*100

  #data# HPSp, HPSe_nV, HPSe_V
  #inits#
  #monitor# Se_V, Se_nV, Sp, pi, OR, VE
}
"


res_prior_check <- run.jags(
  bm_1t_prior_check,
  n.chains = 3,
  inits = list(inits1, inits2, inits3)
  )
res_prior_check

plot(
  res_prior_check,
  plot.type = c("h", "t", "au"),
  vars = c("OR", "pi"),
  layout = c(3, 3)
  )