findbetaqq2(percentile.value1 = )

##Example 1
##We believe that 20% of the units in an area/region have a prevalence of
##disease/infection less than or equal to 0.30 while at the same time we are 90%
##certain that the prevalence is less than 0.60

# The sensitivity of was 0.68 (95\% PrI = 0.63 - 0.73), while the specificity was 
# 0.99 (95\% PrI =  0.98 - 1.00).
bm_1test_nondif <- " model {

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

#data# m, N, y, HPSe, HPSp
#inits#
#monitor# Se, Sp, pi, OR
}
"


HPSe <- findbetaqq2(percentile.value1=0.63, percentile1=0.025,
            percentile.value2=0.73, percentile2=0.975)
HPSe
v <- rbeta(n=1000, HPSe[1], HPSe[2])
q <- quantile(v, probs = c(0.025, 0.5, 0.975))
round(q, 2)
HPSp <- findbetaqq2(percentile.value1=0.98, percentile1=0.025,
                    percentile.value2=0.999, percentile2=0.975)
HPSp
v <- rbeta(n=1000, HPSp[1], HPSp[2])
q <- quantile(v, probs = c(0.025, 0.5, 0.975))
round(q, 2)


bm_1test_nondif

sim_ve_1t_diff()

y = matrix(c(51220, 251541,
         57,3817), nrow = 2, byrow = T)
m = 2
N = apply(y, 1, sum)

results <- autorun.jags(
  bm_1test_nondif,
  data = list(
    N = N,
    y = y,
    HPSe = HPSe,
    HPSp = HPSp
  ),
  inits = list(inits1, inits2, inits3),
  progress.bar = 'none',
  silent.jags = T
)
results

bm_1test_perf <- " model {

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

results <- autorun.jags(
  bm_1test_perf,
  data = list(
    N = N,
    y = y
  ),
  inits = list(inits1, inits2, inits3),
  progress.bar = 'none',
  silent.jags = T
)
results
