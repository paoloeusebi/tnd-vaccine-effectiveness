library(dplyr)
library(ggplot2)
library(cowplot)
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
    Crude = raw_OR,
    Coverage = if_else(true_OR<Upper95 & true_OR>Lower95, 1, 0)
  ) %>%
  group_by(n, p1, true_OR, Sp_V, Sp_nV, Se_V, Se_nV) %>%
  arrange(diff_misc)
write.csv(res1, "tfls/simulations_1t_summary.csv", row.names = F)

# plots
res2 <- res1 %>%
  pivot_longer(bias_bm:bias_raw, names_to = "Model", values_to = "Bias") %>%
  mutate(
    Model = if_else(Model=="bias_raw", "Unadjusted", "Adjusted"),
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
  geom_boxplot(outlier.shape = NA) +
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
  geom_boxplot(outlier.shape = NA) +
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

pcol <- plot_grid(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"),
  labels = c("A", "B"),
  align = 'v',
  hjust = -1,
  ncol = 1
)
pcol

# extract the legend from one of the plots
legend <- get_legend(p1 + theme(legend.position = "bottom"))

p <- plot_grid(legend, pcol, nrow = 2,
          rel_heights = c(0.25, 3.25))
p
ggsave(filename = "tfls/t1_bias_panel.eps", plot = p, width = 6, height = 9)

res3 <- res1 %>%
  mutate(
    Scenario = paste(
      paste0("Sp=", sprintf("%0.3f", Sp_V),"\n"),
      paste0("Se V+=", sprintf("%0.3f", Se_V),"\n"),
      paste0("Se V-=", sprintf("%0.3f", Se_nV)),
      true_OR)) %>%
  group_by(Scenario) %>%
  summarise(mean = mean(Coverage), n = n())
res3
range(res3$mean)


res3a <- res1 %>%
  mutate(
    true_OR_label = paste0("OR=", true_OR),
    Scenario = paste("Scenario:",
      paste0("Sp=", sprintf("%0.3f", Sp_V),";"),
      paste0("Se (V+)=", sprintf("%0.3f", Se_V),";"),
      paste0("Se (V-)=", sprintf("%0.3f", Se_nV),";"),
      true_OR_label),
    Coverage = as.factor(Coverage)) %>%
  group_by(Scenario, true_OR) %>%
  arrange(Lower95) %>%
  mutate(sim_rank=1:1000)

j=0
for (i in names((table(res3a$Scenario)))){
j<-j+1
text<-i # we want to have only the last word "string" with 6 letter
n <- 2 #as the last character will be counted with nchar(), here we discount 1
yt<-as.numeric(substr(x=text,start=nchar(text)-n,stop=nchar(text)))

f <- ggplot(data = res3a[res3a$Scenario == i, ],
             aes(
               x = sim_rank,
               y = Median,
               ymin = Lower95,
               ymax = Upper95
             )) +
  labs(title = i) +
  geom_pointrange(col="grey50",
    # alpha=0.1,
    fatten = 0.5) +
  geom_hline(aes(yintercept = true_OR), linetype = "dashed") +
  labs(x="", y="OR") +
  scale_y_continuous(breaks = yt) +
  theme_bw()
# f
ggsave(filename = paste0("tfls/t1_bias_sim_", j,".eps"), plot = f, width = 16, height = 9)
}
f
