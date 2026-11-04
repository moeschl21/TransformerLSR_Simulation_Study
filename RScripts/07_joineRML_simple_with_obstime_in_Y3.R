library(dplyr)
library(nlme)
library(joineRML)
library(survival)

data_dir <- "D:/AlterPC/Masterarbeit/Dateien/data_joinerml_testing/testing_data_joinerml"

file <- file.path(
  data_dir,
  "DIVAT_sim_missing_train_100_visit_1000_long_3_seed_1.csv"
)

dat <- read.csv(file)

# Convert event indicator to 0/1
dat$event <- ifelse(dat$event == "True", 1, 0)

# Separate datasets for each longitudinal outcome
dat_Y1 <- dat %>%
  dplyr::filter(!is.na(Y1))

dat_Y2 <- dat %>%
  dplyr::filter(!is.na(Y2))

dat_Y3 <- dat %>%
  dplyr::filter(!is.na(Y3))

# Keep only subjects with at least one observation for each outcome
valid_ids <- Reduce(
  intersect,
  list(
    unique(dat_Y1$id),
    unique(dat_Y2$id),
    unique(dat_Y3$id)
  )
)

dat_Y1 <- dat_Y1 %>%
  dplyr::filter(id %in% valid_ids)

dat_Y2 <- dat_Y2 %>%
  dplyr::filter(id %in% valid_ids)

dat_Y3 <- dat_Y3 %>%
  dplyr::filter(id %in% valid_ids)

# Model specification
formLongFixed <- list(
  "Y1" = Y1 ~ obstime + X1 + X2 + X3,
  "Y2" = Y2 ~ obstime + X1 + X2 + X3,
  "Y3" = Y3 ~ obstime + X1 + X2 + X3
)

formLongRandom <- list(
  "Y1" = ~ obstime | id,
  "Y2" = ~ obstime | id,
  "Y3" = ~ obstime | id
)

formSurv <- Surv(time, event) ~ X1 + X2 + X3

# Fit joint model
model_mjoint <- mjoint(
  formLongFixed = formLongFixed,
  formLongRandom = formLongRandom,
  formSurv = formSurv,
  data = list(dat_Y1, dat_Y2, dat_Y3),
  timeVar = "obstime"
)

summary(model_mjoint)