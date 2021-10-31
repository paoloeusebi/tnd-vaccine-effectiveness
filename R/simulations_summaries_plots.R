# Summaries and plot for 1Test-1Pop simulations ---------------------------
res <- read.csv("tfls/simulations_1_test.csv")

res1 <- res %>%
  as_tibble() %>%
  filter(parameter=="OR") %>%
  group_by(n_scenario, n, Se, Sp, p1, true_OR) %>%
  summarize(median_OR_BM = median(Median),
            median_OR_raw = median(raw_OR))
res1
write.csv(res1, "tfls/simulations_1_test_summary.csv", row.names = FALSE)

res2 <- res %>%
  as_tibble() %>%
  filter(parameter=="OR") %>%
  mutate(BM = Median,
         Crude = raw_OR) %>%
  pivot_longer(BM:Crude, names_to = "Model", values_to = "OR") %>%
  mutate(Se_label=paste0("Se=", sprintf("%0.2f", Se)),
         Sp_label=paste0("Sp=", sprintf("%0.2f", Sp)),
         true_OR_label=paste0("OR=", true_OR),
         n=paste0("n=", sprintf("%0.0f", n)))

p <- ggplot(data = res2[res2$p1==0.1 & res2$OR<1,],
            aes(x=Model, y=OR)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = true_OR), linetype="dashed") +
  facet_grid(n + true_OR_label ~ Se_label + Sp_label)+
  theme_bw() +
  scale_y_continuous(breaks=c(0.25, 0.5, 0.75))
png("tfls/simulations_1_test_boxplot_p1_10pct.png", 
    width = 480, height = 480*2)
print(p)
dev.off()

p <- ggplot(data = res2[res2$p1==0.2 & res2$OR<1,],
            aes(x=Model, y=OR)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = true_OR), linetype="dashed") +
  facet_grid(n + true_OR_label ~ Se_label + Sp_label) +
  theme_bw() +
  scale_y_continuous(breaks=c(0.25, 0.5, 0.75))
png("tfls/simulations_1_test_boxplot_p1_20pct.png", 
    width = 480, height = 480*2)
print(p)
dev.off()


# Summaries and plot for 2Tests-2Pops simulations -------------------------
res <- read.csv("tfls/simulations_2_tests.csv")

res1 <- res %>%
  as_tibble() %>%
  filter(parameter=="OR") %>%
  group_by(n_scenario, n, Se1, Sp1, Se2, Sp2, p1, true_OR) %>%
  summarize(median_OR_BM = median(Median),
            median_OR_raw = median(raw_OR1))
res1
write.csv(res1, "tfls/simulations_2_tests_summary.csv", row.names = FALSE)


res2 <- res %>%
  as_tibble() %>%
  filter(parameter=="OR") %>%
  mutate(BM = Median,
         Crude = raw_OR1) %>%
  pivot_longer(BM:Crude, names_to = "Model", values_to = "OR") %>%
  mutate(Se1_label=paste0("Se1=", sprintf("%0.2f", Se1)),
         Sp1_label=paste0("Sp1=", sprintf("%0.2f", Sp1)),
         Se2_label=paste0("Se2=", sprintf("%0.2f", Se2)),
         Sp2_label=paste0("Sp2=", sprintf("%0.2f", Sp2)),
         true_OR_label=paste0("OR=", true_OR),
         n=paste0("n=", sprintf("%0.0f", n)))

for (pct in c(10, 20)){
  for (k in c(10, 100)){
    p <- ggplot(data = res2[res2$p1==(pct/100) & res2$n==paste0("n=",k,"000") & res2$OR<1,],
               aes(x=Model, y=OR)) +
      geom_boxplot() +
      geom_hline(aes(yintercept = true_OR), linetype="dashed") +
      facet_grid(true_OR_label +  Se1_label + Sp1_label ~ Se2_label + Sp2_label)+
      theme_bw() +
      scale_y_continuous(breaks=c(0.25, 0.5, 0.75))
    
    png(paste0("tfls/simulations_2_tests_boxplot_p1_",pct,"pct_n_",k,"k.png"), 
        width = 480, height = 480*2)
    print(p)
    dev.off()
  }
  }