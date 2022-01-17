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


HPSe <- findbetaqq2(
  percentile.value1 = 0.63,
  percentile1 = 0.025,
  percentile.value2 = 0.73,
  percentile2 = 0.975
  )
HPSe
round(qbeta(c(0.025, 0.5, 0.975), # check
            HPSe[1], HPSe[2]), 2)

HPSp <-
  findbeta2(
    themedian = 0.99,
    percentile = 0.975,
    lower.v = FALSE,
    percentile.value = 0.98
  )

HPSp

round(qbeta(c(0.025, 0.5, 0.975), # check
            HPSp[1], HPSp[2]), 3)



# BM incorporating imperfect Se/Sp ----------------------------------------

# data
y <- matrix(c(51220, 251541,
              57,3817), nrow = 2, byrow = T)
m <- 2
N <- apply(y, 1, sum)


bm_1t <- " model {

for (i in 1:m) {

# likelihood
y[i,1:2] ~ dmulti(prob[i,1:2], N[i])
prob[i,1] <- pi[i]*Se + (1-pi[i])*(1-Sp)
prob[i,2] <- pi[i]*(1-Se) + (1-pi[i])*Sp
# priors for prevalence parameters
pi[i] ~ dbeta(1,1)
}

Se~dbeta(HPSe[1], HPSe[2])
Sp~dbeta(HPSp[1], HPSp[2])

# logitSp~dnorm(HPSp[1], HPSp[2])
# Sp <- ilogit(logitSp)

OR = (pi[2]/(1-pi[2])) / (pi[1]/(1-pi[1]))
VE = (1-OR)*100

#data# m, N, y, HPSe, HPSp
#inits#
#monitor# Se, Sp, pi, OR, VE
}
"

res_bm <- run.jags(
  bm_1t,
  n.chains = 3,
  inits = list(inits1, inits2, inits3),
  burnin = 15000,
  sample = 60000,
  )

res_bm

plot(
  res_bm,
  plot.type = c("h","t", "au"),
  vars = c("OR", "VE", "Sp", "Se"),
  layout = c(4, 3)
  )



# BM assuming perfect test ------------------------------------------------

bm_1t_perfect <- " model {

for (i in 1:m) {

# likelihood
y[i,1:2] ~ dmulti(prob[i,1:2], N[i])
prob[i,1] <- pi[i]
prob[i,2] <- (1-pi[i])
# priors for prevalence parameters
pi[i] ~ dbeta(1,1)
}

OR = (pi[2]/(1-pi[2])) / (pi[1]/(1-pi[1]))
VE = (1-OR)*100

#data# m, N, y
#inits#
#monitor# pi, OR, VE
}
"

res_bm_perfect <- run.jags(
  bm_1t_perfect,
  n.chains = 3,
  inits = list(inits1, inits2, inits3),
  burnin = 15000,
  sample = 60000
)

res_bm_perfect

plot(
  res_bm_perfect,
  plot.type = c("h","t", "au"),
  vars = c("OR", "VE"),
  layout = c(2, 3)
)

