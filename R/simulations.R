library(rootSolve)

## initial values
inits1 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 100022)
inits2 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 300022)
inits3 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 500022)

# sample_size=1000; p1=0.01; Se=0.75; Sp = 0.75; true_OR=0.25
n_sim <- 1000 # number of simulations


res <- NULL # initialization 
j <- 0

for (n in c(1000, 10000)){
  for (p1 in c(0.025, 0.05)){
    for (Se in c(0.75, 0.95)){
      for (Sp in c(0.75, 0.95)){
        for (true_OR in c(0.25, 0.75)){
        j <- j+1
          for (i in 1:n_sim){
        
        d <- sim_ve_1_imperfect_test(n = n,
                                base_dis_prev = p1,
                                true_OR = true_OR,
                                covariates = F,
                                Se = Se,
                                Sp = Sp)
        y <- d$data
        m = 2
        N = apply(y, 1, sum)
        
        HPSe <- findbetaqq2(percentile.value1 = (Se-0.025), percentile1=0.05,
                           percentile.value2 = (Se+0.025), percentile2=0.95)
        HPSp <- findbetaqq2(percentile.value1 = (Sp-0.025), percentile1=0.05, 
                           percentile.value2 = (Sp+0.025), percentile2=0.95)
        ## run
        results <- run.jags(
          bm_1test_nondif,
          n.chains = 3,
          inits = list(inits1, inits2, inits3),
          burnin = 1000,
          sample = 5000,
          
        )
        
        res_i <- round(results$summaries, 3) %>% 
          as.data.frame() %>%
          rownames_to_column() %>%
          cbind(sample_size, p1, Se, Sp, true_OR, raw_OR=d$`estimated OR`, n_scenario=j, n_simulation=i) %>%
          rename(parameter=rowname)
        
          
        res <- rbind(res, res_i)
          }
        }
      }
    }
  }
}

res