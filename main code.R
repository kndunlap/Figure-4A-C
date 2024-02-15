library(tidyverse)
science <- read.csv("exp4a.csv")

### This code below is awesome - just need to work on aesthetics now.  

### Cit
science |>
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
  ggplot(aes(x = Concentration, y = mean, color = Genotype)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = Genotype)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 3) +
  geom_vline(xintercept = 40, linetype = "dashed", color = "black") +
  labs(x = "Citrulline [µM]", y = "Doublings Per Day", title = "SLC7A5") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color = "black"))

### Leu
science |>
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
  ggplot(aes(x = Concentration, y = mean, color = Genotype)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = Genotype)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 3) +
  geom_vline(xintercept = 160, linetype = "dashed", color = "black") +
  labs(x = "Leucine [µM]", y = "Doublings Per Day", title = "SLC7A5") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(color = "black"),
        legend.position = "none")



### Phe
science |>
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
  ggplot(aes(x = Concentration, y = mean, color = Genotype)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = Genotype)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 3) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black") +
  labs(x = "Phenylalanine [µM]", y = "Doublings Per Day", title = "SLC7A5") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), axis.line = element_line(color = "black"),
        legend.position = "none")

p1 + p2 + p3 + plot_layout(ncol = 1)
