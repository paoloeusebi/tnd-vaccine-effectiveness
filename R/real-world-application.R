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

OR = (pi[2]/(1-pi[2])) / (pi[1]/(1-pi[1]))
VE = (1-OR)*100

#data# m, N, y, HPSe, HPSp
#inits#
#monitor# Se, Sp, pi, OR, VE
}
"


HPSe <- findbetaqq2(percentile.value1=0.63, percentile1=0.025,
            percentile.value2=0.73, percentile2=0.975)
HPSe
round(qbeta(c(0.025, 0.5, 0.975), # check
            HPSe[1], HPSe[2]), 2)

HPSp <- findbetaqq2(percentile.value1=0.98, percentile1=0.025,
                    percentile.value2=0.99999999, percentile2=0.975)
HPSp
round(qbeta(c(0.025, 0.5, 0.975), # check
            HPSp[1], HPSp[2]), 2)

y = matrix(c(51220, 251541,
         57,3817), nrow = 2, byrow = T)
m = 2
N = apply(y, 1, sum)

results <- autorun.jags(bm_1t, n.chains = 3)
results

plot(results, plot.type=c("trace","histogram", "autocorr"), vars = c("OR"))

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

#data# m, N, y
#inits#
#monitor# pi, OR
}
"

results <- autorun.jags(bm_1t_perfect, n.chains = 3)
results
