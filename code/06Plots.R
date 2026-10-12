
# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggh4x)

fig.path <- "results/plots/"

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

sim.results |> 
  filter(measure != "CITL") |> 
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
ggsave("plot-age-results.tiff", path = fig.path, height = 10, width = 8, dpi = 600)

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
  scale_color_gradient(low = grey(0.2), high = grey(0.8))
ggsave("plot-age-results-grey.tiff", path = fig.path, height = 10, width = 8, dpi = 600)



# FWIT CIT Variation Plot -------------------------------------------------

load("results/tables/ResultsFWIT-CIT.RData")

sim.results |> 
  filter(measure != "CITL") |> 
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
ggsave("plot-fwit-cit-results.tiff", path = fig.path, height = 10, width = 8, dpi = 600)

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
  scale_color_gradient(low = grey(0.2), high = grey(0.8)) +
  theme_bw()
ggsave("plot-fwit-cit-results-grey.tiff", path = fig.path, height = 10, width = 8, dpi = 600)



# Retp proportion variation Plot ------------------------------------------

load("results/tables/ResultsRetp.RData")
sim.results |>
  filter(measure != "CITL") |> 
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
ggsave("plot-retp-results.tiff", path = fig.path, height = 10, width = 8, dpi = 600)



# Sample Size Variation Plot ----------------------------------------------

load("results/tables/ResultsSampleSize.RData")
sim.results |> 
  filter(measure != "CITL") |> 
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
ggsave("plot-sample-size-results.tiff", path = fig.path, height = 10, width = 8, dpi = 600)


# Sensitivity analyses ----------------------------------------------------

sensitivity_analysis_path <- "results/sens/"

#################
## Net benefit ##
#################

# Sample size
load("results/tables/ResultsSampleSize-sens.RData")
(sens.nb.samplesize <- sens.results |> 
  filter(str_detect(measure, "NB")) |> 
  mutate(
    measure = factor(measure,
                     levels = c("NB", "NBDall", "NB.7", "NBDall.7", "NB.75", "NBDall.75"),
                     labels = c("Net benefit to transplant none-0.8", "Net benefit to transplant all-0.8",
                                "Net benefit to transplant none-0.7", "Net benefit to transplant all-0.7",
                                "Net benefit to transplant none-0.75", "Net benefit to transplant all-0.75")),
    outcome = factor(outcome, levels = c("UKDCD_dichot", "Swisstransplant"),
                     labels = c("UK DCD", "Swisstransplant"))
  ) |> 
  separate(measure, into = c("Measure", "Threshold probability"), sep = "-") |> 
  mutate(`Threshold probability` = as.factor(`Threshold probability`)) |> 
  ggplot(aes(x = sample_size, y = median, color = `Threshold probability`,
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_point() +
  geom_errorbar() +
  facet_grid(outcome~Measure) +
  khroma::scale_colour_light() +
  labs(x = "Sample size", y = "Net benefit",
       color = "Threshold probability") +
  theme_bw())
ggsave("NB-sample_size.png", path = sensitivity_analysis_path, width = 8, height = 4, dpi = 300)

# Retp
load("results/tables/ResultsRetp-sens.RData")
(sens.nb.retp <- sens.results |> 
  filter(str_detect(measure, "NB")) |> 
  mutate(
    measure = factor(measure,
                     levels = c("NB", "NBDall", "NB.7", "NBDall.7", "NB.75", "NBDall.75"),
                     labels = c("Net benefit to transplant none-0.8", "Net benefit to transplant all-0.8",
                                "Net benefit to transplant none-0.7", "Net benefit to transplant all-0.7",
                                "Net benefit to transplant none-0.75", "Net benefit to transplant all-0.75")),
    outcome = factor(outcome, levels = c("UKDCD_dichot", "Swisstransplant"),
                     labels = c("UK DCD", "Swisstransplant"))
  ) |> 
  separate(measure, into = c("Measure", "Threshold probability"), sep = "-") |> 
  mutate(`Threshold probability` = as.factor(`Threshold probability`)) |> 
  ggplot(aes(x = retp_prob, y = median, color = `Threshold probability`,
             ymin = lower, ymax = upper)) +
  geom_line() +
  geom_point() +
  geom_errorbar() +
  facet_grid(outcome~Measure) +
  khroma::scale_colour_light() +
  labs(x = "Retransplantation proportion", y = "Net benefit",
       color = "Threshold probability") +
  theme_bw())
ggsave("NB-retp.png", path = sensitivity_analysis_path, width = 8, height = 4, dpi = 300)

# Combined plot
cowplot::plot_grid(
  sens.nb.samplesize +
    ggtitle("Net benefit sensitivity analysis w/ changing sample size") +
    theme(legend.position = "bottom"),
  sens.nb.retp +
    ggtitle("Net benefit sensitivity analysis w/ changing retransplantation proportion") +
    theme(legend.position = "bottom"),
  nrow = 2,
  labels = "AUTO",
  label_size = 20
)
ggsave("NB-comb.png", path = sensitivity_analysis_path, width = 8, height = 10, dpi = 300)

#####################################
## Calibration slope and intercept ##
#####################################

# Sample size
load("results/tables/ResultsSampleSize-sens.RData")
(sens.cali.samplesize <- sens.results |> 
    filter(str_detect(measure, "calibration"),
           sample_size >= 50) |>
    mutate(
      measure = factor(measure,
                       levels = c("calibration.intercept", "calibration.slope",
                                  "calibration.intercept.logit", "calibration.slope.logit"),
                       labels = c("Calibration intercept-linear", "Calibration slope-linear",
                                  "Calibration intercept-logit", "Calibration slope-logit")),
      outcome = factor(outcome, levels = c("UKDCD_dichot", "Swisstransplant"),
                       labels = c("UK DCD", "Swisstransplant"))
    ) |> 
    separate(measure, into = c("Measure", "Probability transformation"), sep = "-") |> 
    mutate(`Probability transformation` = as.factor(`Probability transformation`)) |> 
    ggplot(aes(x = sample_size, y = median, color = `Probability transformation`,
               ymin = lower, ymax = upper)) +
    geom_line() +
    geom_point() +
    geom_errorbar() +
    facet_grid(outcome~Measure) +
    khroma::scale_colour_light() +
    scale_color_discrete(breaks = c("linear", "logit"),
                         labels = list("linear", expression(logit^-1))) +
    labs(x = "Sample size", y = "Calibration",
         color = "Probability transformation") +
    theme_bw())
ggsave("calibration-sample_size.png", path = sensitivity_analysis_path, width = 8, height = 4, dpi = 300)

# Retp
load("results/tables/ResultsRetp-sens.RData")
(sens.cali.retp <- sens.results |> 
    filter(str_detect(measure, "calibration")) |>
    mutate(
      measure = factor(measure,
                       levels = c("calibration.intercept", "calibration.slope",
                                  "calibration.intercept.logit", "calibration.slope.logit"),
                       labels = c("Calibration intercept-linear", "Calibration slope-linear",
                                  "Calibration intercept-logit", "Calibration slope-logit")),
      outcome = factor(outcome, levels = c("UKDCD_dichot", "Swisstransplant"),
                       labels = c("UK DCD", "Swisstransplant"))
    ) |> 
    separate(measure, into = c("Measure", "Probability transformation"), sep = "-") |> 
    mutate(`Probability transformation` = as.factor(`Probability transformation`)) |> 
    ggplot(aes(x = retp_prob, y = median, color = `Probability transformation`,
               ymin = lower, ymax = upper)) +
    geom_line() +
    geom_point() +
    geom_errorbar() +
    facet_grid(outcome~Measure) +
    khroma::scale_colour_light() +
    scale_color_discrete(breaks = c("linear", "logit"),
                         labels = list("linear", expression(logit^-1))) +
    labs(x = "Retransplantation proportion", y = "Calibration",
         color = "Probability transformation") +
    theme_bw())
ggsave("calibration-retp.png", path = sensitivity_analysis_path, width = 8, height = 4, dpi = 300)

# Combined plot
cowplot::plot_grid(
  sens.cali.samplesize +
    ggtitle("Calibration sensitivity analysis w/ changing sample size") +
    theme(legend.position = "bottom"),
  sens.cali.retp +
    ggtitle("Calibration sensitivity analysis w/ changing retransplantation proportion") +
    theme(legend.position = "bottom"),
  nrow = 2,
  labels = "AUTO",
  label_size = 20
)
ggsave("calibration-comb.png", path = sensitivity_analysis_path, width = 8, height = 10, dpi = 300)

