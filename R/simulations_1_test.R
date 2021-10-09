## initial values
inits1 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 100022)
inits2 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 300022)
inits3 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 500022)

# n=10000; p1=0.01; Se=0.75; Sp = 0.75; true_OR=0.25 for debugging
n_sim <- 100 # number of simulations


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
          inits = list(inits1, inits2, inits3),
          progress.bar='none', silent.jags = T
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

res <- read.csv("tfls/simulations_1_test.csv")

res1 <- res %>%
  as.tibble() %>%
  filter(parameter=="OR") %>%
  mutate(bias_OR1 = ((Median/true_OR)-1)*100,
         bias_OR2 = ((raw_OR/true_OR)-1)*100) %>%
  group_by(n_scenario, n, Se, Sp, p1, true_OR) %>%
  summarize(bias_OR1_median = median(bias_OR1),
            bias_OR2_median = median(bias_OR2))
res1
write.csv(res1, "tfls/simulations_1_test_summary.csv", row.names = FALSE)

res2 <- res %>%
  as.tibble() %>%
  filter(parameter=="OR") %>%
  mutate(bias_OR1 = ((Median/true_OR)-1)*100,
         bias_OR2 = ((raw_OR/true_OR)-1)*100) %>%
  pivot_longer(bias_OR1:bias_OR2, names_to = "bias_OR", values_to = "bias") %>%
  mutate(Se=paste("Se=", Se),
         Sp=paste("Sp=", Sp),
         true_OR=paste("OR=", true_OR),
         n=paste("n=", n))

p <- ggplot(data = res2[res2$p1==0.025,],
       aes(x=bias_OR, y=bias)) +
  geom_boxplot() +
  facet_grid(true_OR + n ~ Se + Sp,
             scales="free_y")
png("tfls/simulations_1_test_boxplot_p1_025.png", 
    width = 480, height = 480)
print(p)
dev.off()

p <- ggplot(data = res2[res2$p1==0.05,],
       aes(x=bias_OR, y=bias)) +
  geom_boxplot() +
  facet_grid(true_OR + n ~ Se + Sp,
             scales="free_y")
png("tfls/simulations_1_test_boxplot_p1_050.png", 
    width = 480, height = 480)
print(p)
dev.off()
