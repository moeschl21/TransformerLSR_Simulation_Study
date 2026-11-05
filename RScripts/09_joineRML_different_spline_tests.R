library(dplyr)
library(joineRML)
library(survival)
library(splines)

data_dir <- "D:/AlterPC/Masterarbeit/Dateien/13_master_data_joinerml/data_joinerml_train"

files <- file.path(
  data_dir,
  paste0(
    "DIVAT_sim_missing_train_100_visit_1000_long_3_seed_",
    1:5,
    ".csv"
  )
)


# Survival model
formSurv <- Surv(time, event) ~ X1 + X2 + X3

# Random-effects structure
formLongRandom <- list(
  "Y1" = ~ obstime | id,
  "Y2" = ~ obstime | id,
  "Y3" = ~ 1 | id
)


# Fixed effects: linear time effect
formLongFixed_linear <- list(
  "Y1" = Y1 ~ obstime + X1 + X2 + X3,
  "Y2" = Y2 ~ obstime + X1 + X2 + X3,
  "Y3" = Y3 ~ obstime + X1 + X2 + X3
)


# Fixed effects: natural cubic spline df = 2
formLongFixed_df2 <- list(
  "Y1" = Y1 ~ ns(obstime, df = 2) + X1 + X2 + X3,
  "Y2" = Y2 ~ ns(obstime, df = 2) + X1 + X2 + X3,
  "Y3" = Y3 ~ ns(obstime, df = 2) + X1 + X2 + X3
)


# Fixed effects: natural cubic spline df = 3
formLongFixed_df3 <- list(
  "Y1" = Y1 ~ ns(obstime, df = 3) + X1 + X2 + X3,
  "Y2" = Y2 ~ ns(obstime, df = 3) + X1 + X2 + X3,
  "Y3" = Y3 ~ ns(obstime, df = 3) + X1 + X2 + X3
)


# Storage
results <- list()

fits_linear <- list()
fits_df2 <- list()
fits_df3 <- list()


for (seed in 1:5) {
  
  # Load data
  dat <- read.csv(files[seed])
  
  dat <- dat %>%
    arrange(id, visit)
  
  
  if (is.character(dat$event)) {
    dat$event <- dat$event == "True"
  }
  
  
  # Remove missing values separately for each outcome
  dat_Y1 <- dat %>%
    dplyr::filter(!is.na(Y1))
  
  dat_Y2 <- dat %>%
    dplyr::filter(!is.na(Y2))
  
  dat_Y3 <- dat %>%
    dplyr::filter(!is.na(Y3))
  

  # Identify subjects with at least one measurement  
  valid_ids <- Reduce(
    intersect,
    list(
      unique(dat_Y1$id),
      unique(dat_Y2$id),
      unique(dat_Y3$id)
    )
  )
  
  
  # Subjects excluded because one complete longitudinal
  # outcome is missing
  excluded_ids <- setdiff(
    unique(dat$id),
    valid_ids
  )
   
  
  # Keep only valid subjects
  dat_Y1 <- dat_Y1 %>%
    filter(id %in% valid_ids) %>%
    select(
      id,
      obstime,
      Y1,
      X1,
      X2,
      X3
    )
  
  dat_Y2 <- dat_Y2 %>%
    filter(id %in% valid_ids) %>%
    select(
      id,
      obstime,
      Y2,
      X1,
      X2,
      X3
    )
  
  dat_Y3 <- dat_Y3 %>%
    filter(id %in% valid_ids) %>%
    select(
      id,
      obstime,
      Y3,
      X1,
      X2,
      X3
    )
  
  
  # Survival data
  surv_dat <- dat %>%
    filter(id %in% valid_ids) %>%
    distinct(id, .keep_all = TRUE) %>%
    select(
      id,
      time,
      event,
      X1,
      X2,
      X3
    )
  
  
  # Longitudinal data list for mjoint
  dat_long <- list(
    "Y1" = dat_Y1,
    "Y2" = dat_Y2,
    "Y3" = dat_Y3
  )
  
  
  # Information after missing-value removal
  n_subjects <- length(valid_ids)
  
  cat("\nLongitudinal observations used:\n")
  cat("Y1:", nrow(dat_Y1), "\n")
  cat("Y2:", nrow(dat_Y2), "\n")
  cat("Y3:", nrow(dat_Y3), "\n")
  
  cat("\nSubjects per outcome:\n")
  cat("Y1:", length(unique(dat_Y1$id)), "\n")
  cat("Y2:", length(unique(dat_Y2$id)), "\n")
  cat("Y3:", length(unique(dat_Y3$id)), "\n")
  
  
  # Fit model: LINEAR
  set.seed(12345)
  
  fit_linear <- mjoint(
    formLongFixed = formLongFixed_linear,
    formLongRandom = formLongRandom,
    formSurv = formSurv,
    data = dat_long,
    survData = surv_dat,
    timeVar = "obstime",
    verbose = TRUE
  )
  
  
  # Fit model: SPLINE df = 2
  fit_df2 <- mjoint(
    formLongFixed = formLongFixed_df2,
    formLongRandom = formLongRandom,
    formSurv = formSurv,
    data = dat_long,
    survData = surv_dat,
    timeVar = "obstime",
    verbose = TRUE
  )
  
  
  # Fit model: SPLINE df = 3
  fit_df3 <- mjoint(
    formLongFixed = formLongFixed_df3,
    formLongRandom = formLongRandom,
    formSurv = formSurv,
    data = dat_long,
    survData = surv_dat,
    timeVar = "obstime",
    verbose = TRUE
  )
  
  
  # Store fitted models
  fits_linear[[seed]] <- fit_linear
  fits_df2[[seed]] <- fit_df2
  fits_df3[[seed]] <- fit_df3
  
  
  # Extract log-likelihoods
  ll_linear <- logLik(fit_linear)
  ll_df2 <- logLik(fit_df2)
  ll_df3 <- logLik(fit_df3)
  
  logLik_linear <- as.numeric(ll_linear)
  logLik_df2 <- as.numeric(ll_df2)
  logLik_df3 <- as.numeric(ll_df3)
  
  
  # Number of parameters
  k_linear <- attr(ll_linear, "df")
  k_df2 <- attr(ll_df2, "df")
  k_df3 <- attr(ll_df3, "df")
  
  
  # AIC
  AIC_linear <- AIC(fit_linear)
  AIC_df2 <- AIC(fit_df2)
  AIC_df3 <- AIC(fit_df3)
  
  
  # Subject-based BIC
  BIC_linear <- -2 * logLik_linear +
    k_linear * log(n_subjects)
  
  BIC_df2 <- -2 * logLik_df2 +
    k_df2 * log(n_subjects)
  
  BIC_df3 <- -2 * logLik_df3 +
    k_df3 * log(n_subjects)
  
  
  # Store results
  results[[seed]] <- data.frame(
    
    seed = seed,
    
    n_subjects = n_subjects,
    n_excluded = length(excluded_ids),
    
    n_Y1 = nrow(dat_Y1),
    n_Y2 = nrow(dat_Y2),
    n_Y3 = nrow(dat_Y3),
    
    logLik_linear = logLik_linear,
    logLik_df2 = logLik_df2,
    logLik_df3 = logLik_df3,
    
    AIC_linear = AIC_linear,
    AIC_df2 = AIC_df2,
    AIC_df3 = AIC_df3,
    
    BIC_linear = BIC_linear,
    BIC_df2 = BIC_df2,
    BIC_df3 = BIC_df3
  )
  
  
  # Print results for current seed
  cat("\n\n")
  cat("============================================================\n")
  cat("RESULT: SEED", seed, "\n")
  cat("============================================================\n\n")
  
  cat("                 Linear         df = 2         df = 3\n")
  cat("----------------------------------------------------------\n")
  
  cat(
    "logLik:     ",
    round(logLik_linear, 3),
    "     ",
    round(logLik_df2, 3),
    "     ",
    round(logLik_df3, 3),
    "\n"
  )
  
  cat(
    "AIC:        ",
    round(AIC_linear, 3),
    "     ",
    round(AIC_df2, 3),
    "     ",
    round(AIC_df3, 3),
    "\n"
  )
  
  cat(
    "BIC:        ",
    round(BIC_linear, 3),
    "     ",
    round(BIC_df2, 3),
    "     ",
    round(BIC_df3, 3),
    "\n"
  )
}



# Combine all results
results_df <- bind_rows(results)


# Final comparison table
comparison <- results_df %>%
  select(
    seed,
    n_subjects,
    n_excluded,
    n_Y1,
    n_Y2,
    n_Y3,
    
    logLik_linear,
    logLik_df2,
    logLik_df3,
    
    AIC_linear,
    AIC_df2,
    AIC_df3,
    
    BIC_linear,
    BIC_df2,
    BIC_df3
  )


cat("\n\n")
cat("============================================================\n")
cat("FINAL COMPARISON: LINEAR vs df = 2 vs df = 3\n")
cat("============================================================\n\n")

print(comparison)


# Compact table
comparison_thesis <- results_df %>%
  transmute(
    Dataset = seed,
    
    AIC_linear = round(AIC_linear, 3),
    BIC_linear = round(BIC_linear, 3),
    
    AIC_df2 = round(AIC_df2, 3),
    BIC_df2 = round(BIC_df2, 3),
    
    AIC_df3 = round(AIC_df3, 3),
    BIC_df3 = round(BIC_df3, 3)
  )

print(comparison_thesis)


# Summary: Which specification has lowest AIC / BIC?
best_AIC <- apply(
  results_df[, c("AIC_linear", "AIC_df2", "AIC_df3")],
  1,
  function(x) c("Linear", "df=2", "df=3")[which.min(x)]
)

best_BIC <- apply(
  results_df[, c("BIC_linear", "BIC_df2", "BIC_df3")],
  1,
  function(x) c("Linear", "df=2", "df=3")[which.min(x)]
)


cat("\n\n")
cat("============================================================\n")
cat("BEST SPECIFICATION PER DATASET\n")
cat("============================================================\n\n")

for (seed in 1:5) {
  
  cat(
    "Dataset", seed,
    ": lowest AIC =", best_AIC[seed],
    "| lowest BIC =", best_BIC[seed],
    "\n"
  )
}
