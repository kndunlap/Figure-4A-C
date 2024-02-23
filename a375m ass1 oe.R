library(tidyverse)
library(patchwork)
science <- read.csv("exp4a.csv")

### Cit
p1 <- science |>
  drop_na() |> 
  mutate_at(c("WT", "KO"), as.numeric) |>
  rename(Metabolite = Leu,
         Concentration = X,
         WT = c(X.1, X.2, WT, X.3),
         KO = c(X.4, X.5, KO, X.6)) |> 
  pivot_longer(
    cols = (WT1:KO4), 
    names_to = "Genotype",
    values_to = "dpd") |> 
  mutate(Genotype = gsub("\\d", "", Genotype)) |>
  group_by(Metabolite, Concentration, Genotype) |>
  summarize(mean = mean(dpd), sd = sd(dpd)) |> 
  filter(Metabolite == "Cit") |>
  mutate(Concentration = Concentration + 0.5) |>
  ggplot(aes(x = Concentration, y = mean, color = Genotype)) + 
  geom_point() +
  geom_smooth(method="lm", formula = y ~ log(x), se = FALSE) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 3) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "black") +
  labs(x = "Citrulline [µM]", y = "Doublings Per Day") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.position = "none") + 
  scale_color_manual(values = c("WT" = "red","KO" = "blue"), breaks = c("WT", "KO"))

### Leu
p2 <- science |>
  drop_na() |> 
  mutate_at(c("WT", "KO"), as.numeric) |>
  rename(Metabolite = Leu,
         Concentration = X,
         WT = c(X.1, X.2, WT, X.3),
         KO = c(X.4, X.5, KO, X.6)) |> 
  pivot_longer(
    cols = (WT1:KO4), 
    names_to = "Genotype",
    values_to = "dpd") |> 
  mutate(Genotype = gsub("\\d", "", Genotype)) |>
  group_by(Metabolite, Concentration, Genotype) |>
  summarize(mean = mean(dpd), sd = sd(dpd)) |> 
  filter(Metabolite == "Leu") |> 
  mutate(Concentration = Concentration + 0.5) |>
  ggplot(aes(x = Concentration, y = mean, color = Genotype)) + 
  geom_point() +
  geom_smooth(method="lm", formula = y ~ log(x), se = FALSE) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 3) +
  geom_vline(xintercept = 160, linetype = "dashed", color = "black") +
  labs(x = "Leucine [µM]", y = "Doublings Per Day", title = "Cell Line: A375m ASS1-OE") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size = 22),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16)) +
  scale_color_manual(values = c("WT" = "red","KO" = "blue"), breaks = c("WT", "KO"))



### Phe
p3 <- science |>
  drop_na() |> 
  mutate_at(c("WT", "KO"), as.numeric) |>
  rename(Metabolite = Leu,
         Concentration = X,
         WT = c(X.1, X.2, WT, X.3),
         KO = c(X.4, X.5, KO, X.6)) |> 
  pivot_longer(
    cols = (WT1:KO4), 
    names_to = "Genotype",
    values_to = "dpd") |> 
  mutate(Genotype = gsub("\\d", "", Genotype)) |>
  group_by(Metabolite, Concentration, Genotype) |>
  summarize(mean = mean(dpd), sd = sd(dpd)) |> 
  filter(Metabolite == "Phe") |> 
  mutate(Concentration = Concentration + 0.5) |>
  ggplot(aes(x = Concentration, y = mean, color = Genotype)) + 
  geom_point() +
  geom_smooth(method="lm", formula = y ~ log(x), se = FALSE) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 3) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black") +
  labs(x = "Phenylalanine [µM]", y = "Doublings Per Day") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.position = "none") + 
  scale_color_manual(values = c("WT" = "red","KO" = "blue"), breaks = c("WT", "KO"))


plots_a375 <- p2 + p3 + p1 + plot_layout(ncol = 1)

print(plots_a375)

ggsave("a375m ass1 oe.jpg", plot = plots_a375, dpi = 300, width = 5.3, height = 6.5, units = "in")
