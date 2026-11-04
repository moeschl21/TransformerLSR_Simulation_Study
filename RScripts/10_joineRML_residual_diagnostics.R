library(dplyr)
library(nlme)
library(splines)


# Load data

data_dir <- "D:/AlterPC/Masterarbeit/Dateien/13_master_data_joinerml/data_joinerml_train"

file <- file.path(
  data_dir,
  "DIVAT_sim_missing_train_100_visit_1000_long_3_seed_1.csv"
)

dat <- read.csv(file)


# Separate datasets for the three longitudinal outcomes

dat_Y1 <- dat %>%
  filter(!is.na(Y1))

dat_Y2 <- dat %>%
  filter(!is.na(Y2))

dat_Y3 <- dat %>%
  filter(!is.na(Y3))


# ORIGINAL MODELS

fit_Y1_original <- lme(
  Y1 ~ ns(obstime, df = 3) + X1 + X2 + X3,
  random = ~ obstime | id,
  data = dat_Y1
)

fit_Y2_original <- lme(
  Y2 ~ ns(obstime, df = 3) + X1 + X2 + X3,
  random = ~ obstime | id,
  data = dat_Y2
)

fit_Y3_original <- lme(
  Y3 ~ ns(obstime, df = 3) + X1 + X2 + X3,
  random = ~ 1 | id,
  data = dat_Y3
)


# Box-Cox transformation

boxcox_transform <- function(y, lambda) {
  
  if (abs(lambda) < 1e-8) {
    return(log(y))
  }
  
  return((y^lambda - 1) / lambda)
}


# Box-Cox requires positive values

if (any(dat_Y1$Y1 <= 0)) {
  stop("Y1 contains values <= 0.")
}

if (any(dat_Y2$Y2 <= 0)) {
  stop("Y2 contains values <= 0.")
}

if (any(dat_Y3$Y3 <= 0)) {
  stop("Y3 contains values <= 0.")
}


# Lambda grid

lambda_grid <- seq(-2, 2, by = 0.05)


# Determine lambda for Y1

logLik_Y1 <- rep(NA, length(lambda_grid))

for (i in seq_along(lambda_grid)) {
  
  lambda <- lambda_grid[i]
  
  dat_Y1$Y1_BC <- boxcox_transform(
    dat_Y1$Y1,
    lambda
  )
  
  fit_temp <- lme(
    Y1_BC ~ ns(obstime, df = 3) + X1 + X2 + X3,
    random = ~ obstime | id,
    data = dat_Y1,
    method = "ML"
  )
  
  logLik_Y1[i] <-
    as.numeric(logLik(fit_temp)) +
    (lambda - 1) * sum(log(dat_Y1$Y1))
}

lambda_Y1 <- lambda_grid[
  which.max(logLik_Y1)
]


# Determine lambda for Y2

logLik_Y2 <- rep(NA, length(lambda_grid))

for (i in seq_along(lambda_grid)) {
  
  lambda <- lambda_grid[i]
  
  dat_Y2$Y2_BC <- boxcox_transform(
    dat_Y2$Y2,
    lambda
  )
  
  fit_temp <- lme(
    Y2_BC ~ ns(obstime, df = 3) + X1 + X2 + X3,
    random = ~ obstime | id,
    data = dat_Y2,
    method = "ML"
  )
  
  logLik_Y2[i] <-
    as.numeric(logLik(fit_temp)) +
    (lambda - 1) * sum(log(dat_Y2$Y2))
}

lambda_Y2 <- lambda_grid[
  which.max(logLik_Y2)
]


# Determine lambda for Y3

logLik_Y3 <- rep(NA, length(lambda_grid))

for (i in seq_along(lambda_grid)) {
  
  lambda <- lambda_grid[i]
  
  dat_Y3$Y3_BC <- boxcox_transform(
    dat_Y3$Y3,
    lambda
  )
  
  fit_temp <- lme(
    Y3_BC ~ ns(obstime, df = 3) + X1 + X2 + X3,
    random = ~ 1 | id,
    data = dat_Y3,
    method = "ML"
  )
  
  logLik_Y3[i] <-
    as.numeric(logLik(fit_temp)) +
    (lambda - 1) * sum(log(dat_Y3$Y3))
}

lambda_Y3 <- lambda_grid[
  which.max(logLik_Y3)
]


# Apply Box-Cox transformations

dat_Y1$Y1_BC <- boxcox_transform(
  dat_Y1$Y1,
  lambda_Y1
)

dat_Y2$Y2_BC <- boxcox_transform(
  dat_Y2$Y2,
  lambda_Y2
)

dat_Y3$Y3_BC <- boxcox_transform(
  dat_Y3$Y3,
  lambda_Y3
)


# BOX-COX MODELS

fit_Y1_BC <- lme(
  Y1_BC ~ ns(obstime, df = 3) + X1 + X2 + X3,
  random = ~ obstime | id,
  data = dat_Y1
)

fit_Y2_BC <- lme(
  Y2_BC ~ ns(obstime, df = 3) + X1 + X2 + X3,
  random = ~ obstime | id,
  data = dat_Y2
)

fit_Y3_BC <- lme(
  Y3_BC ~ ns(obstime, df = 3) + X1 + X2 + X3,
  random = ~ 1 | id,
  data = dat_Y3
)


# Residuals

res_Y1_original <- residuals(
  fit_Y1_original,
  type = "normalized"
)

res_Y2_original <- residuals(
  fit_Y2_original,
  type = "normalized"
)

res_Y3_original <- residuals(
  fit_Y3_original,
  type = "normalized"
)


res_Y1_BC <- residuals(
  fit_Y1_BC,
  type = "normalized"
)

res_Y2_BC <- residuals(
  fit_Y2_BC,
  type = "normalized"
)

res_Y3_BC <- residuals(
  fit_Y3_BC,
  type = "normalized"
)


# Fitted values

fitted_Y1_original <- fitted(fit_Y1_original)
fitted_Y2_original <- fitted(fit_Y2_original)
fitted_Y3_original <- fitted(fit_Y3_original)

fitted_Y1_BC <- fitted(fit_Y1_BC)
fitted_Y2_BC <- fitted(fit_Y2_BC)
fitted_Y3_BC <- fitted(fit_Y3_BC)


# FIGURE 1: ORIGINAL SCALE

png(
  filename = file.path(data_dir, "diagnostics_original_seed_1.png"),
  width = 2400,
  height = 3000,
  res = 300
)

par(
  mfrow = c(3, 2),
  mar = c(4.5, 4.5, 3, 1)
)


# Y1: Q-Q Plot

qqnorm(
  res_Y1_original,
  main = "Y1: Q-Q Plot",
  xlab = "Theoretical Quantiles",
  ylab = "Normalized Residuals"
)

qqline(res_Y1_original)


# Y1: Residuals vs Fitted

plot(
  fitted_Y1_original,
  res_Y1_original,
  main = "Y1: Residuals vs Fitted",
  xlab = "Fitted Values",
  ylab = "Normalized Residuals"
)

abline(h = 0, lty = 2)


# Y2: Q-Q Plot

qqnorm(
  res_Y2_original,
  main = "Y2: Q-Q Plot",
  xlab = "Theoretical Quantiles",
  ylab = "Normalized Residuals"
)

qqline(res_Y2_original)


# Y2: Residuals vs Fitted

plot(
  fitted_Y2_original,
  res_Y2_original,
  main = "Y2: Residuals vs Fitted",
  xlab = "Fitted Values",
  ylab = "Normalized Residuals"
)

abline(h = 0, lty = 2)


# Y3: Q-Q Plot

qqnorm(
  res_Y3_original,
  main = "Y3: Q-Q Plot",
  xlab = "Theoretical Quantiles",
  ylab = "Normalized Residuals"
)

qqline(res_Y3_original)


# Y3: Residuals vs Fitted

plot(
  fitted_Y3_original,
  res_Y3_original,
  main = "Y3: Residuals vs Fitted",
  xlab = "Fitted Values",
  ylab = "Normalized Residuals"
)

abline(h = 0, lty = 2)


dev.off()


# FIGURE 2: BOX-COX TRANSFORMED SCALE

png(
  filename = file.path(data_dir, "diagnostics_boxcox_seed_1.png"),
  width = 2400,
  height = 3000,
  res = 300
)

par(
  mfrow = c(3, 2),
  mar = c(4.5, 4.5, 3, 1)
)


# Y1: Q-Q Plot

qqnorm(
  res_Y1_BC,
  main = paste0(
    "Y1: Q-Q Plot (lambda = ",
    round(lambda_Y1, 2),
    ")"
  ),
  xlab = "Theoretical Quantiles",
  ylab = "Normalized Residuals"
)

qqline(res_Y1_BC)


# Y1: Residuals vs Fitted

plot(
  fitted_Y1_BC,
  res_Y1_BC,
  main = "Y1: Residuals vs Fitted",
  xlab = "Fitted Values",
  ylab = "Normalized Residuals"
)

abline(h = 0, lty = 2)


# Y2: Q-Q Plot

qqnorm(
  res_Y2_BC,
  main = paste0(
    "Y2: Q-Q Plot (lambda = ",
    round(lambda_Y2, 2),
    ")"
  ),
  xlab = "Theoretical Quantiles",
  ylab = "Normalized Residuals"
)

qqline(res_Y2_BC)


# Y2: Residuals vs Fitted

plot(
  fitted_Y2_BC,
  res_Y2_BC,
  main = "Y2: Residuals vs Fitted",
  xlab = "Fitted Values",
  ylab = "Normalized Residuals"
)

abline(h = 0, lty = 2)


# Y3: Q-Q Plot

qqnorm(
  res_Y3_BC,
  main = paste0(
    "Y3: Q-Q Plot (lambda = ",
    round(lambda_Y3, 2),
    ")"
  ),
  xlab = "Theoretical Quantiles",
  ylab = "Normalized Residuals"
)

qqline(res_Y3_BC)


# Y3: Residuals vs Fitted

plot(
  fitted_Y3_BC,
  res_Y3_BC,
  main = "Y3: Residuals vs Fitted",
  xlab = "Fitted Values",
  ylab = "Normalized Residuals"
)

abline(h = 0, lty = 2)


dev.off()