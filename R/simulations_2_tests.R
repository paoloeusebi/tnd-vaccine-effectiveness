## initial values
inits1 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 100022)
inits2 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 300022)
inits3 = list(".RNG.name" ="base::Mersenne-Twister", ".RNG.seed" = 500022)

# n=10000; p1=0.01; Se1=0.75; Sp1 = 0.75; true_OR=0.25 for debugging
n_sim <- 100 # number of simulations


res <- NULL # initialization 
j <- 0

for (n in c(10000)) {
  for (p1 in c(0.025, 0.05)) {
    for (Se1 in c(0.75, 0.95)) {
      for (Sp1 in c(0.75, 0.95)) {
        for (Se2 in c(0.75, 0.95)) {
          for (Sp2 in c(0.75, 0.95)) {
            for (true_OR in c(0.25, 0.75)) {
              j <- j + 1
              for (i in 1:n_sim) {
                d <- sim_ve_2_imperfect_tests(
                  n = n,
                  base_dis_prev = p1,
                  true_OR = true_OR,
                  covariates = F,
                  Se1 = Se1, Sp1 = Sp1,
                  Se2 = Sp2, Sp2 = Sp2
                )
                
                y <- d$data
                m = 2
                N = apply(y, 1, sum)
                HPSe = matrix(c(1,1,1,1), nrow = 2)
                HPSp = matrix(c(1,1,1,1), nrow = 2)
                
                ## run
                results <- run.jags(
                  blcm_2test_nondif,
                  inits = list(inits1, inits2, inits3),
                  progress.bar = 'none',
                  silent.jags = T
                )
                
                res_i <- round(results$summaries, 3) %>%
                  as.data.frame() %>%
                  rownames_to_column() %>%
                  cbind(
                    n,
                    p1,
                    Se1, Se2,
                    Sp1, Sp2,
                    true_OR,
                    raw_OR1 = d$`estimated OR T1`, 
                    raw_OR2 = d$`estimated OR T2`,
                    n_scenario = j,
                    n_simulation = i
                  ) %>%
                  rename(parameter = rowname)
                
                res <- rbind(res, res_i)
                
                print(paste("Simulation", i, "- Scenario", j))
              }
            }
          }
        }
      }
    }
  }
}

write.csv(res, "tfls/simulations_2_tests.csv", row.names = FALSE)

res <- read.csv("tfls/simulations_2_tests.csv")

res1 <- res %>%
  as.tibble() %>%
  filter(parameter=="OR") %>%
  mutate(bias_OR1 = ((Median/true_OR)-1)*100,
         bias_OR2 = ((raw_OR1/true_OR)-1)*100) %>%
  group_by(n_scenario, n, Se1, Sp1, Se2, Sp2, p1, true_OR) %>%
  summarize(bias_OR1_median = median(bias_OR1),
            bias_OR2_median = median(bias_OR2))
res1
write.csv(res1, "tfls/simulations_2_tests_summary.csv", row.names = FALSE)

res2 <- res %>%
  as.tibble() %>%
  filter(parameter=="OR") %>%
  mutate(bias_OR1 = ((Median/true_OR)-1)*100,
         bias_OR2 = ((raw_OR1/true_OR)-1)*100) %>%
  pivot_longer(bias_OR1:bias_OR2, names_to = "bias_OR", values_to = "bias") %>%
  mutate(Se1=paste("Se1=", Se1),
         Sp1=paste("Sp1=", Sp1),
         Se2=paste("Se2=", Se2),
         Sp2=paste("Sp2=", Sp2),
         true_OR=paste("OR=", true_OR))

p <- ggplot(data = res2[res2$p1==0.025,],
       aes(x=bias_OR, y=bias)) +
  geom_boxplot() +
  facet_grid(true_OR + Se1 + Sp1 ~ Se2 + Sp2,
             scales="free_y")
png("tfls/simulations_2_tests_boxplot_p1_025.png", 
    width = 480, height = 960)
print(p)
dev.off()

p <- ggplot(data = res2[res2$p1==0.05,],
       aes(x=bias_OR, y=bias)) +
  geom_boxplot() +
  facet_grid(true_OR + Se1 + Sp1 ~ Se2 + Sp2,  scales="free_y")
png("tfls/simulations_2_tests_boxplot_p1_050.png",
    width = 480, height = 960)
print(p)
dev.off()
