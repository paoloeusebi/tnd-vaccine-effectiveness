# Summaries and plot for 1Test-1Pop simulations ---------------------------
res <- read.csv("tfls/simulations_1t.csv")

# simulation summaries
res1 <- res %>%
  filter(parameter == "OR") %>%
  mutate(
    diff_misc = Sp_V - Sp_nV,
    bias_bm = round(Median / true_OR, 2),
    bias_raw = round(raw_OR / true_OR, 2),
    BM = Median,
    Crude = raw_OR
  ) %>%
  group_by(n, p1, true_OR, Sp_V, Sp_nV, Se_V, Se_nV) %>%
  arrange(diff_misc)
write.csv(res1, "tfls/simulations_1t_summary.csv", row.names = F)

# plots
res2 <- res1 %>%
  pivot_longer(BM:Crude, names_to = "Model", values_to = "OR") %>%
  mutate(
    Scenario = paste(
      paste0("Sp V+=", sprintf("%0.2f", Sp_V),"\n"),
      paste0("Sp V-=", sprintf("%0.2f", Sp_nV),"\n")
    ),
    Misclassification = paste("Differential \n misclassification \n", ifelse(diff_misc==0, "No", "Yes")),
    true_OR_label = paste0("OR=", true_OR),
    n = paste0("n=", sprintf("%0.0f", n))
  )

p <- ggplot(data = res2[res2$OR < 1, ],
            aes(x = Model, y = OR)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = true_OR), linetype = "dashed") +
  facet_grid(n + true_OR_label ~ Misclassification+ Scenario) +
  theme_bw() +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1))
p
