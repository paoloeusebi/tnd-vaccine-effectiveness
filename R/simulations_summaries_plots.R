# Summaries and plot for 1Test-1Pop simulations ---------------------------
res <- read.csv("tfls/simulations_1t.csv")

# simulation summaries
res1 <- res %>%
  filter(parameter == "OR") %>%
  mutate(
    diff_misc = Se_V - Se_nV,
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
  pivot_longer(bias_bm:bias_raw, names_to = "Model", values_to = "Bias") %>%
  mutate(
    Model = if_else(Model=="bias_raw", "Crude", "BM"),
    Scenario = paste(
      paste0("Sp=", sprintf("%0.3f", Sp_V),"\n"),
      paste0("Se V+=", sprintf("%0.3f", Se_V),"\n"),
      paste0("Se V-=", sprintf("%0.3f", Se_nV))
    ),
    Misclassification = paste("Differential \n misclassification \n", ifelse(diff_misc==0, "No", "Yes")),
    true_OR_label = paste0("OR=", true_OR),
    n = paste0("n=", sprintf("%0.0f", n))
  )

p1 <- ggplot(data = res2[res2$diff_misc==0,],
            aes(x = Model, y = Bias, fill=Model)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  facet_grid(true_OR_label ~ Scenario) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank()) +
  labs(x="") +
  scale_y_continuous(breaks = seq(1,10,0.25))
p1
ggsave(filename = "tfls/t1_bias_nd.eps", plot = p1, width = 6, height = 4)

p2 <- ggplot(data = res2[res2$diff_misc!=0,],
             aes(x = Model, y = Bias, fill=Model)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  facet_grid(true_OR_label ~ Scenario) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank()) +
  labs(x="") +
  scale_y_continuous(breaks = seq(1,10,0.25))
p2
ggsave(filename = "tfls/t1_bias_d.eps", plot = p2, width = 6, height = 4)