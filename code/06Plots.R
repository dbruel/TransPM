
# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggh4x)



# Graphical Abstract Plot -------------------------------------------------

# Load the data
load("results/tables/ResultsRetp.RData")

# create the plot
pdf("results/plots/plot-graphical-abstract.pdf", height = 2.5, width = 3)
sim.results |> 
  filter(measure == "AUC" & outcome == "Swisstransplant") |>
  ggplot(aes(retp_prob, median, ymin = lower, ymax = upper)) +
  geom_line(alpha = 0.2) +
  geom_point(size = 3, color = "white") +
  geom_point() +
  geom_errorbar() +
  labs(
    x = "Retransplantation probability",
    y = "AUC"
  ) +
  theme_minimal()
dev.off()



# Age variation plot ------------------------------------------------------

load("results/tables/ResultsAgeShift.RData")
pdf("results/plots/plot-age-results.pdf", height = 10, width = 8)
sim.results |> 
  mutate(
    outcome = factor(outcome,
                     levels = c("UKDCD_dichot", "Swisstransplant"),
                     labels = c("UK DCD", "Swisstransplant")),
    measure = recode(measure,
                     `Net benefit to transplant no one` = "Net benefit to \ntransplant none",
                     `Net benefit to transplant all` = "Net benefit to \ntransplant all")
  ) |> 
  ggplot(aes(x = d.age, y = median, col = r.age)) +
  geom_line(alpha = 0.2, aes(group = as.factor(r.age))) +
  geom_point(size = 3, col = "white") +
  geom_point(size = 2) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.2, width = 0) +
  # facet_grid(measure ~ outcome, scales = "free_y") +
  ggh4x::facet_grid2(measure ~ outcome, scales = "free_y", independent = "y") +
  labs(lty = NULL, col = expression(mu[R.age]),
       x = expression(mu[D.age]), y = NULL) +
  # geom_vline(aes(xintercept = 57.8, lty = "Swiss \npopulation \nmean")) +
  theme_bw() +
  scale_color_viridis_c()
dev.off()



# FWIT CIT Variation Plot -------------------------------------------------

load("results/tables/ResultsFWIT-CIT.RData")
pdf("results/plots/plot-fwit-cit-results.pdf", height = 10, width = 8)
sim.results |> 
  mutate(
    outcome = factor(outcome,
                     levels = c("UKDCD_dichot", "Swisstransplant"),
                     labels = c("UK DCD", "Swisstransplant")),
    measure = recode(measure,
                     `Net benefit to transplant no one` = "Net benefit to \ntransplant none",
                     `Net benefit to transplant all` = "Net benefit to \ntransplant all")
  ) |> 
  ggplot(aes(x = FWIT, y = median, col = CITg6)) +
  facet_grid2(measure ~ outcome, scales = "free_y", independent = "y") +
  geom_line(alpha = 0.2, aes(group = as.factor(CITg6))) +
  geom_point(size = 3, col = "white") +
  geom_point(size = 2) +
  # geom_vline(aes(xintercept = 0.702, lty = "Swiss \npopulation \nmean")) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, alpha = 0.5) +
  labs(x = expression(mu[FWIT]),
       y = NULL,
       col = expression(p[CIT > 6*h]),
       lty = NULL) +
  scale_color_viridis_c() +
  theme_bw()
dev.off()



# Retp proportion variation Plot ------------------------------------------

load("results/tables/ResultsRetp.RData")
pdf("results/plots/plot-retp-results.pdf", height = 10, width = 8)
sim.results |>
  mutate(
    outcome = factor(outcome,
                     levels = c("UKDCD_dichot", "Swisstransplant"),
                     labels = c("UK DCD", "Swisstransplant")),
    measure = recode(measure,
                     `Net benefit to transplant no one` = "Net benefit to \ntransplant none",
                     `Net benefit to transplant all` = "Net benefit to \ntransplant all")
  ) |> 
  ggplot(aes(x = retp_prob, y = median)) +
  geom_line(alpha = 0.2) +
  geom_point(size = 3, col = "white") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .01, alpha = 0.5) +
  # geom_vline(aes(xintercept = 0.02, color = "Swiss target population mean")) +
  facet_grid2(measure~outcome, scales = "free_y", independent = "y") +
  labs(x = expression(p[retp]),
       y = NULL,
       col = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()



# Sample Size Variation Plot ----------------------------------------------

load("results/tables/ResultsSampleSize.RData")
pdf("results/plots/plot-sample-size-results.pdf", height = 10, width = 8)
sim.results |> 
  mutate(
    outcome = factor(outcome,
                     levels = c("UKDCD_dichot", "Swisstransplant"),
                     labels = c("UK DCD", "Swisstransplant")),
    measure = recode(measure,
                     `Net benefit to transplant no one` = "Net benefit to \ntransplant none",
                     `Net benefit to transplant all` = "Net benefit to \ntransplant all")
  ) |> 
  ggplot(aes(x = sample_size, y = median)) +
  geom_line(alpha = 0.2) +
  geom_point(size = 3, col = "white") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 50) +
  facet_grid2(measure ~ outcome, scales = "free_y", independent = "y") +
  labs(x = "Sample size",
       y = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()