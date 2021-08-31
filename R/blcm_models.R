blcm_1test_nondif <- " model {

for (i in 1:m) {

                # likelihood

                y[i,1:2] ~ dmulti(prob[i,1:2],n[i])
                prob[i,1] <- pi[i]*Se + (1-pi[i])*(1-Sp)
                prob[i,2] <- pi[i]*(1-Se) + (1-pi[i])*Sp
                
                # priors for prevalence parameters
                pi[i] ~ dbeta(1,1)
                }

Se ~ dbeta(1,1)T(1-Sp, )
Sp ~ dbeta(1,1)

OR = (pi[2]/(1-pi[2])) / (pi[1]/(1-pi[1]))

#data# m, n, y
#inits#
#monitor# Se, Sp, pi, OR
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

blcm_2test_dif <- " model {

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