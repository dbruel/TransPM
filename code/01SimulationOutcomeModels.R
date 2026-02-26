
#-------------------------------------------------------------------

library(SimDesign)
library(pROC)
library(tidyverse)
library(kableExtra)
library(tmvtnorm)

Design <- createDesign(outcome = c("UKDCD_dichot", "UKDCD_cont", "UCLA", "KC", "Swisstransplant"))

n.sim <- 1000

#-------------------------------------------------------------------

Generate <- function(condition, fixed_objects) {
  n <- 1200
  baseline.prev <- 0.006
  baseline.prev.cont <- 0.000003
  baseline.surv.ucla <- 0.945
  baseline.surv.kc <- 0.84
  baseline.prev.st <- 0.06
  
  d.age_bmi <- rtmvnorm(n, mean = c(57.1, 26),
                        sigma  = matrix(c(16.1^2, 0.1*sqrt(16.1^2*4.3^2),
                                          0.1*sqrt(16.1^2*4.3^2), 4.3^2),
                                        nrow = 2, ncol = 2),
                        lower = c(0, 0), upper = c(Inf, Inf),
                        algorithm = "gibbs")
  d.age <- d.age_bmi[, 1]
  d.bmi <- d.age_bmi[, 2]
  fwit <- rtruncate(n, rnorm, c(0, Inf), mean = 26.9, sd = 9.4)
  cit <- factor(rbinom(n, 1, 0.702), levels = c(0, 1), labels = c("<=6h", ">6h"))
  r.age <- rtruncate(n, rnorm, c(0, Inf), mean = 57.8, sd = 9.9)
  r.meld <- rtruncate(n, rpois, c(6, 40), lambda = 14.7)
  retp <- factor(rbinom(n, 1, 0.02), levels = c(0, 1), labels = c(FALSE, TRUE))
  
  ## Simulation using UK DCD Model with the dichotomized values similar to point scheme
  if(condition$outcome == "UKDCD_dichot"){
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
  
  ## Simulation of UK DCD Model with continuous values
  if(condition$outcome == "UKDCD_cont"){
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
    refs <- c(0, 46, 23, 15, 0, 52, 14, 0)
    mat <- sweep(mat, 2, refs, "-") # Subtract reference vector from each row
    betas <- c(log(baseline.prev.cont/(1-baseline.prev.cont)), 0.084, 0.519, 
               0.341, log(1/0.791), 0.241, 0.109, 8.571)
    probs <- mat %*% betas |>
      plogis()
    failure <- factor(rbinom(n, 1, probs), levels = c(0, 1), labels = c(FALSE, TRUE))
  }
  
  ## Simulation using UCLA Model
  if(condition$outcome == "UCLA"){
    mat <- cbind(
      fwit > 20,
      cit == ">6h",
      retp == TRUE
    )
    betas <- c(1.9, 2.7, 11.6) |> 
      log()
    probs <- 1 - baseline.surv.ucla^exp(mat %*% betas)
    failure <- factor(rbinom(n, 1, probs), levels = c(0, 1), labels = c(FALSE, TRUE))
  }
  
  ## Simulation using Kings College Model
  if(condition$outcome == "KC"){
    mat <- cbind(
      fwit > 25,
      r.meld > 25,
      retp == TRUE
    )
    betas <- c(1.48, 2.75, 1.87) |> 
      log()
    probs <- 1 - baseline.surv.kc^exp(mat %*% betas)
    failure <- factor(rbinom(n, 1, probs), levels = c(0, 1), labels = c(FALSE, TRUE))
  }
  
  ## Simulation using Swisstransplant Model
  if(condition$outcome == "Swisstransplant"){
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
  failure.rate <- sum(outcomes)/nrow(dat)
  mean.r.meld <- mean(dat$r.meld)
  
  ret <- nc(AUC = AUC,
            calibration.slope = calibration.slope,
            calibration.intercept = calibration.intercept,
            NB = NB,
            NBDall = NBDall,
            failure.rate = failure.rate,
            mean.r.meld = mean.r.meld)
  ret
}

Summarise <- function(condition, results, fixed_objects) {
  # median <- median(results)
  # lower <- quantile(results, 0.025)
  # upper <- quantile(results, 0.975)
  # MCse <- sqrt(var(results)/n.sim)
  # 
  # ret <- c(median = median, lower = lower, upper = upper, MCse = MCse)
  ret <- results
  ret
}

#-------------------------------------------------------------------

# Simulation run ----------------------------------------------------------

res <- runSimulation(design=Design, replications=n.sim, generate=Generate, 
                     analyse=Analyse, summarise=Summarise, seed = rep(8, nrow(Design)))
sim.object <- SimExtract(res, what = 'summarise') # saves the results


# Missingness report ------------------------------------------------------

sim.object |> 
  bind_rows() |>
  cbind(Design[rep(1:nrow(Design), each = n.sim)]) |> 
  group_by(outcome) |> 
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
  write.csv("results/missingness/outcome.csv", row.names = FALSE)

# Results -----------------------------------------------------------

# Sanity checks
mean.r.meld <- sim.object |> 
  bind_rows() |> 
  select(mean.r.meld) |>
  slice(1:n.sim) |> 
  c() |> 
  unlist() |> 
  unname()
save(mean.r.meld, file = "results/checks/mean-r-meld.RData")
failure.rates <- sim.object |> 
  bind_rows() |> 
  select(failure.rate) |> 
  cbind(Design[rep(1:nrow(Design), each = n.sim)]) |> 
  mutate(
    outcome = factor(outcome,
                     levels = c("UKDCD_dichot", "UKDCD_cont",
                                "UCLA", "KC", "Swisstransplant"),
                     labels = c("UK DCD dichotomized",
                                "UK DCD continuous",
                                "UCLA", "King's College",
                                "Swisstransplant"))
  )
save(failure.rates, file = "results/checks/failure-rates.RData")

# Summarize the results
sim.results <- sim.object |> 
  bind_rows() |> 
  select(AUC, calibration.slope, calibration.intercept, NB, NBDall) |> 
  gather(key = "measure", value = "value", everything()) |> 
  cbind(Design[rep(1:nrow(Design), each = n.sim)]) |> 
  mutate(
    measure = factor(measure,
                     levels = c("calibration.intercept", "calibration.slope",
                                "NB", "NBDall", "AUC"),
                     labels = c("Calibration intercept", "Calibration slope",
                                "Net benefit to transplant no one",
                                "Net benefit to transplant all",
                                "AUC")),
    outcome = factor(outcome,
                     levels = c("UKDCD_dichot", "UKDCD_cont",
                                "UCLA", "KC", "Swisstransplant"),
                     labels = c("UK DCD dichotomized",
                                "UK DCD continuous",
                                "UCLA", "King's College",
                                "Swisstransplant"))
  ) |> 
  group_by(outcome, measure) |> 
  summarize(median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975),
            MCse = sqrt(var(value)/n.sim),
            n_sim = n()) |> 
  ungroup()

# Result Table
save(sim.results, file = "results/tables/ResultsOutcomeModels.RData")
suppressWarnings(
  kable(sim.results, format = "html", digits = 3) |> 
    kable_styling(full_width = F, position = "left") |> 
    save_kable("results/tables/ResultsOutcomeModels.html", self_contained = TRUE)
)
