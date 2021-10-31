## initial values
inits1 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 100022)
inits2 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 300022)
inits3 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 500022)

# for debugging
# n=10000; p1=0.1; Se=0.95; Sp = 0.9; true_OR=0.25; vax_prob = 0.75 

n_sim <- 100 # number of simulations

res <- NULL # initialization 
j <- 0

set.seed(1234)

for (n in c(10000, 100000)){
  for (p1 in c(0.1, 0.2)){
    for (Se in c(0.90, 0.95)){
      for (Sp in c(0.90, 0.95)){
        for (true_OR in c(0.25, 0.5)){
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
        
        HPSe <- findbetaqq2(percentile.value1 = (Se-0.025), percentile1 =0.025,
                            percentile.value2 = (Se+0.025), percentile2 = 0.975)
        HPSp <- findbetaqq2(percentile.value1 = (Sp-0.025), percentile1 = 0.025,
                            percentile.value2 = (Sp+0.025), percentile2 = 0.975)
        
        ## run
        results <- run.jags(
          bm_1test_nondif,
          inits = list(inits1, inits2, inits3),
          progress.bar='none', silent.jags = T,
          burnin = 10000,
          thin = 10,
          sample = 10000
        )

        res_i <- round(results$summaries, 3) %>% 
          as.data.frame() %>%
          rownames_to_column() %>%
          cbind(n, p1, Se, Sp, true_OR, raw_OR=d$`estimated OR`, n_scenario=j, n_simulation=i) %>%
          rename(parameter=rowname)
        
        res <- rbind(res, res_i)
        
        print(paste("Simulation",i,"- Scenario", j))
          }
        }
      }
    }
  }
}

write.csv(res, "tfls/simulations_1_test.csv", row.names = FALSE)