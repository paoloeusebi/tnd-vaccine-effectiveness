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


Se_V[1] ~ dbeta(1, 1)T(1-Sp_V[1], )
Sp_V[1] ~ dbeta(1,1)
Se_V[2] ~ dbeta(1, 1)T(1-Sp_V[2], )
Sp_V[2] ~ dbeta(1,1)

Se_nV[1] ~ dbeta(1, 1)T(1-Sp_nV[1], )
Sp_nV[1] ~ dbeta(1,1)
Se_nV[2] ~ dbeta(1, 1)T(1-Sp_nV[2], )
Sp_nV[2] ~ dbeta(1,1)

OR = (pi[2]/(1-pi[2])) / (pi[1]/(1-pi[1]))

#data# m, n, y
#inits#
#monitor# Se_V, Sp_V, Se_nV, Sp_nV, pi, OR
}

"

blcm_2test_nondif <- " model {

for (i in 1:m) {

                # likelihood

                y[i,1:4] ~ dmulti(prob[i,1:4],n[i])
                prob[i,1] <- pi[i]*(Se[1]*Se[2]) + (1-pi[i])*((1-Sp[1])*(1-Sp[2]))
                prob[i,2] <- pi[i]*(Se[1]*(1-Se[2])) + (1-pi[i])*((1-Sp[1])*Sp[2])
                prob[i,3] <- pi[i]*((1-Se[1])*Se[2]) + (1-pi[i])*(Sp[1]*(1-Sp[2]))
                prob[i,4] <- pi[i]*((1-Se[1])*(1-Se[2])) + (1-pi[i])*(Sp[1]*Sp[2])
                # priors for prevalence parameters
                pi[i] ~ dbeta(1,1)
                }

Se[1] ~ dbeta(1, 1)T(1-Sp[1], )
Sp[1] ~ dbeta(1,1)
Se[2] ~ dbeta(1, 1)T(1-Sp[2], )
Sp[2] ~ dbeta(1,1)

OR = (pi[2]/(1-pi[2])) / (pi[1]/(1-pi[1]))

#data# m, n, y
#inits#
#monitor# Se, Sp, pi, OR
}

"