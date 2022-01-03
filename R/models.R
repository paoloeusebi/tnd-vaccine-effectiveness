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


blcm_2test_nondif <- " model {

for (i in 1:m) {

                # likelihood

                y[i,1:4] ~ dmulti(prob[i,1:4], N[i])
                prob[i,1] <- pi[i]*(Se[1]*Se[2]) + (1-pi[i])*((1-Sp[1])*(1-Sp[2]))
                prob[i,2] <- pi[i]*(Se[1]*(1-Se[2])) + (1-pi[i])*((1-Sp[1])*Sp[2])
                prob[i,3] <- pi[i]*((1-Se[1])*Se[2]) + (1-pi[i])*(Sp[1]*(1-Sp[2]))
                prob[i,4] <- pi[i]*((1-Se[1])*(1-Se[2])) + (1-pi[i])*(Sp[1]*Sp[2])
                # priors for prevalence parameters
                pi[i] ~ dbeta(1,1)
                }

Se[1] ~ dbeta(HPSe[1,1], HPSe[1,2])T(1-Sp[1], )
Sp[1] ~ dbeta(HPSp[1,1], HPSp[1,2])
Se[2] ~ dbeta(HPSe[2,1], HPSe[2,2])T(1-Sp[2], )
Sp[2] ~ dbeta(HPSp[2,1], HPSp[2,2])

OR = (pi[2]/(1-pi[2])) / (pi[1]/(1-pi[1]))

#data# m, N, y, HPSe, HPSp
#inits#
#monitor# Se, Sp, pi, OR
}
"

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
  logit_Se_V ~ dnorm(logit(HPSe_V[1]), HPSe_V[2])
  logit_Sp_V ~ dnorm(logit(HPSp_V[1]), HPSp_V[2])
  logit_Se_nV ~ dnorm(logit(HPSe_nV[1]), HPSe_nV[2])
  logit_Sp_nV ~ dnorm(logit(HPSp_nV[1]), HPSp_nV[2])
  
  Se_V <- ilogit(logit_Se_V)
  Sp_V <- ilogit(logit_Sp_V)
  Se_nV <- ilogit(logit_Se_nV)
  Sp_nV <- ilogit(logit_Sp_nV)
  
  # Computing OR
  OR = (pi[2] / (1 - pi[2])) / (pi[1] / (1 - pi[1]))

  #data
  #inits#
  #monitor# Se_V, Sp_V, Se_nV, Sp_nV, pi, OR
  }
"


blcm_2test_dif <- " model {

                # likelihood

                y[1,1:4] ~ dmulti(prob[1,1:4],n[1])
                prob[1,1] <- pi[1]*(Se_V[1]*Se_V[2]) + (1-pi[1])*((1-Sp_V[1])*(1-Sp_V[2]))
                prob[1,2] <- pi[1]*(Se_V[1]*(1-Se_V[2])) + (1-pi[1])*((1-Sp_V[1])*Sp_V[2])
                prob[1,3] <- pi[1]*((1-Se_V[1])*Se_V[2]) + (1-pi[1])*(Sp_V[1]*(1-Sp_V[2]))
                prob[1,4] <- pi[1]*((1-Se_V[1])*(1-Se_V[2])) + (1-pi[1])*(Sp_V[1]*Sp_V[2])
                # priors for prevalence parameters
                pi[1] ~ dbeta(1,1)

                y[2,1:4] ~ dmulti(prob[2,1:4],n[2])
                prob[2,1] <- pi[2]*(Se_nV[1]*Se_nV[2]) + (1-pi[2])*((1-Sp_nV[1])*(1-Sp_nV[2]))
                prob[2,2] <- pi[2]*(Se_nV[1]*(1-Se_nV[2])) + (1-pi[2])*((1-Sp_nV[1])*Sp_nV[2])
                prob[2,3] <- pi[2]*((1-Se_nV[1])*Se_nV[2]) + (1-pi[2])*(Sp_nV[1]*(1-Sp_nV[2]))
                prob[2,4] <- pi[2]*((1-Se_nV[1])*(1-Se_nV[2])) + (1-pi[2])*(Sp_nV[1]*Sp_nV[2])
                # priors for prevalence parameters
                pi[2] ~ dbeta(1,1)


Se_V[1] ~ dbeta(HPSe_V[1,1], HPSe_V[1,2])T(1-Sp_V[1], )
Sp_V[1] ~ dbeta(HPSp_V[1,1], HPSp_V[1,2])
Se_V[2] ~ dbeta(HPSe_V[2,1], HPSe_V[2,2])T(1-Sp_V[2], )
Sp_V[2] ~ dbeta(HPSp_V[2,1], HPSe_V[2,2])

Se_nV[1] ~ dbeta(HPSe_nV[1,1], HPSe_nV[1,2])T(1-Sp_nV[1], )
Sp_nV[1] ~ dbeta(HPSp_nV[1,1], HPSp_nV[1,2])
Se_nV[2] ~ dbeta(HPSe_nV[2,1], HPSe_nV[2,2])T(1-Sp_nV[2], )
Sp_nV[2] ~ dbeta(HPSp_nV[2,1], HPSe_nV[2,2])

OR = (pi[2]/(1-pi[2])) / (pi[1]/(1-pi[1]))

#data# m, n, y, HPSe_V, HPSp_V, HPSe_nV, HPSp_nV
#inits#
#monitor# Se_V, Sp_V, Se_nV, Sp_nV, pi, OR
}
"

