
#-------------------------------------------------------------------

library(SimDesign)
library(tmvtnorm)
library(pROC)
library(tidyverse)
library(kableExtra)

Design <- createDesign(d.age = c(seq(30, 80, 10), 57.1),
                       r.age = c(seq(30, 80, 10), 57.8),
                       outcome = c("UKDCD_dichot", "Swisstransplant"))

n.sim <- 1000

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects) {
  n <- 1200
  d.age.mean <- condition$d.age
  r.age.mean <- condition$r.age
  
  baseline.prev <- 0.007
  baseline.prev.cont <- 0.000003
  baseline.surv.ucla <- 0.945
  baseline.surv.kc <- 0.84
  
  # Predictive Variables
  d.age_bmi <- rtmvnorm(n, mean = c(d.age.mean, 26),
                        sigma  = matrix(c(16.1^2, 0.1*sqrt(16.1^2*4.3^2),
                                          0.1*sqrt(16.1^2*4.3^2), 4.3^2),
                                        nrow = 2, ncol = 2),
                        lower = c(0, 0), upper = c(Inf, Inf),
                        algorithm = "gibbs")
  d.age <- d.age_bmi[, 1]
  d.bmi <- d.age_bmi[, 2]
  fwit <- rtruncate(n, rnorm, c(0, Inf), mean = 26.9, sd = 9.4)
  cit <- factor(rbinom(n, 1, 0.702), levels = c(0, 1), labels = c("<=6h", ">6h"))
  r.age <- rtruncate(n, rnorm, c(0, Inf), mean = r.age.mean, sd = 9.9)
  r.meld <- rtruncate(n, rpois, c(6, 40), lambda = 14.7)
  retp <- factor(rbinom(n, 1, 0.02), levels = c(0, 1), labels = c(FALSE, TRUE))
  
  # Outcomes
  ## Simulation using UK DCD Model with the dichotomized values similar to point scheme
  if(condition$outcome == "UKDCD_dichot"){
    baseline.prev <- 0.006
    mat <- cbind(
      rep(1, n),
      d.age > 60,
      d.bmi > 25,
      fwit > 20,
      cit == ">6h",
      r.age > 60,
      r.meld > 25,
      retp == TRUE
    )
    betas <- c(baseline.prev/(1-baseline.prev), 1/0.084, 1/0.519, 
               1/0.341, 1/0.791, 1/0.241, 1/0.109, 8.571) |> 
      log()
    probs <- mat %*% betas |> 
      plogis()
    failure <- factor(rbinom(n, 1, probs), levels = c(0, 1), labels = c(FALSE, TRUE))
  }
  ## Simulation using Swisstransplant Model
  if(condition$outcome == "Swisstransplant"){
    baseline.prev.st <- 0.06
    mat <- cbind(
      rep(1, n),
      d.age,
      d.bmi,
      fwit,
      cit == ">6h",
      r.age,
      r.meld,
      retp == TRUE
    )
    betas_unshrunk <- c(0.026357, -0.003422, -0.003203, -0.346430, 0.003736, 0.007541, 1.257448)
    betas <- c(log(baseline.prev.st/(1-baseline.prev.st)), betas_unshrunk*0.9)
    probs <- mat %*% betas |>
      plogis()
    failure <- factor(rbinom(n, 1, probs), levels = c(0, 1), labels = c(FALSE, TRUE))
  }
  
  # Data
  dat <- data.frame(
    d.age,
    d.bmi,
    fwit,
    cit,
    r.age,
    r.meld,
    retp,
    failure
  )
  
  dat
}

Analyse <- function(condition, dat, fixed_objects) {
  # Calculate the patient scores
  points <- c(2, 3, 3, 6, 2, 3, 2, 9)
  mat <- cbind(
    dat$d.age > 60,
    dat$d.bmi > 25,
    dat$fwit > 20 & dat$fwit <= 30,
    dat$fwit > 30,
    dat$cit == ">6h",
    dat$r.age > 60,
    dat$r.meld > 25,
    dat$retp == TRUE
  )
  pat_scores <- mat %*% points |> 
    c()
  
  # Get the outcomes
  outcomes <- dat$failure == TRUE
  
  # Calculate the AUC
  AUC <- suppressMessages(auc(outcomes, pat_scores))
  
  # Calibration slope and intercept
  df.calibration <- cut(pat_scores, breaks = c(0, 5, 10, 27), include.lowest = TRUE) |> 
    as.numeric() |> 
    recode(`1` = mean(c(0,5)), `2` = mean(c(6,10)), `3` = mean(c(11,27))) |>
    {\(x) x/27}() |> 
    {\(x) data.frame(prediction.cat = x, outcome = outcomes)}() |> 
    aggregate(outcome ~ prediction.cat, FUN = mean)
  fit <- lm(outcome ~ prediction.cat, data = df.calibration)
  calibration.slope <- fit$coefficients[2] |> unname()
  calibration.intercept <- fit$coefficients[1] |> unname()
  
  # Net Benefit
  pt <- 0.8 # Success Threshold for transplanting
  TP <- sum(!outcomes & pat_scores <= 10)
  FP <- sum(outcomes & pat_scores <= 10)
  NB <- (TP - FP * pt/(1-pt)) / length(outcomes)
  
  # Difference of NB to transplant all and transplant none
  TP_all <- sum(!outcomes)
  FP_all <- sum(outcomes)
  NB_all <- (TP_all - FP_all * pt/(1-pt)) / length(outcomes)
  NBDall <- NB - NB_all
  
  # Sanity checks
  mean.d.age <- mean(dat$d.age)
  mean.r.age <- mean(dat$r.age)
  sd.d.age <- sd(dat$d.age)
  sd.r.age <- sd(dat$r.age)
  cor.age.bmi <- cor(dat$d.age, dat$d.bmi)
  mean.bmi <- mean(dat$d.bmi)
  sd.bmi <- sd(dat$d.bmi)
  
  ret <- nc(AUC = AUC,
            calibration.slope = calibration.slope,
            calibration.intercept = calibration.intercept,
            NB = NB,
            NBDall = NBDall,
            mean.d.age = mean.d.age,
            mean.r.age = mean.r.age,
            sd.d.age = sd.d.age,
            sd.r.age = sd.r.age,
            cor.d.age.bmi = cor.age.bmi,
            mean.d.bmi = mean.bmi,
            sd.d.bmi = sd.bmi)
  ret
}

Summarise <- function(condition, results, fixed_objects) {
  ret <- results
  ret
}



# Simulation run ----------------------------------------------------------

res <- runSimulation(design=Design, replications=n.sim, generate=Generate, 
                     analyse=Analyse, summarise=Summarise, seed = rep(8, nrow(Design)))
sim.object <- SimExtract(res, what = 'summarise')


# Missingness report ------------------------------------------------------

sim.object |> 
  bind_rows() |> 
  cbind(Design[rep(1:nrow(Design), each = n.sim)]) |> 
  group_by(outcome, d.age, r.age) |> 
  summarise(across(everything(), ~sum(is.na(.)))) |> 
  mutate(
    outcome = factor(outcome,
                     levels = c("UKDCD_dichot", "UKDCD_cont",
                                "UCLA", "KC", "Swisstransplant"),
                     labels = c("UK DCD dichotomized",
                                "UK DCD continuous",
                                "UCLA", "King's College",
                                "Swisstransplant"))
  ) |> 
  arrange(outcome) |> 
  write.csv("results/missingness/age.csv", row.names = FALSE)


# Analysis ----------------------------------------------------------------

## Sanity checks ---------------
d.age.checks <- sim.object |> 
  bind_rows() |> 
  select(mean.d.age, sd.d.age, cor.d.age.bmi) |> 
  cbind(Design[rep(1:nrow(Design), each = n.sim)]) |> 
  filter(r.age == 57.8 & outcome == "UKDCD_dichot") |> 
  select(-c(r.age, outcome))
r.age.checks <- sim.object |> 
  bind_rows() |> 
  select(mean.r.age, sd.r.age) |> 
  cbind(Design[rep(1:nrow(Design), each = n.sim)]) |> 
  filter(d.age == 57.1 & outcome == "UKDCD_dichot") |> 
  select(-c(d.age, outcome))
d.bmi.checks <- sim.object |> 
  bind_rows() |> 
  select(mean.d.bmi, sd.d.bmi) |> 
  cbind(Design[rep(1:nrow(Design), each = n.sim)]) |> 
  filter(r.age == 57.8 & outcome == "UKDCD_dichot") |>
  select(-c(r.age, outcome))
save(d.age.checks, d.bmi.checks, r.age.checks, file = "results/checks/age-bmi-checks.RData")

## Summary of simulation results ---------------
sim.results <- sim.object |> 
  bind_rows() |> 
  select(AUC, calibration.slope, calibration.intercept, NB, NBDall) |>
  cbind(Design[rep(1:nrow(Design), each = n.sim)]) |> 
  pivot_longer(cols = -all_of(names(Design)),
               names_to = "measure",
               values_to = "value") |>
  mutate(
    measure = factor(measure,
                     levels = c("calibration.intercept", "calibration.slope",
                                "NB", "NBDall", "AUC"),
                     labels = c("Calibration intercept", "Calibration slope",
                                "Net benefit to transplant no one",
                                "Net benefit to transplant all",
                                "AUC")),
    outcome = factor(outcome,
                     levels = c("UKDCD_dichot", "Swisstransplant"))
  ) |> 
  group_by(d.age, r.age, outcome, measure) |> 
  summarise(median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975),
            MCse = sqrt(var(value)/n.sim),
            n_sim = n()) |> 
  ungroup()


# Results Table -----------------------------------------------------------
save(sim.results, file = "results/tables/ResultsAgeShift.RData")
suppressWarnings(
  kable(sim.results, format = "html", digits = 3) |> 
    kable_styling(full_width = F, position = "left") |> 
    save_kable("results/tables/ResultsAgeShift.html", self_contained = TRUE)
)
