library(dplyr)
library(nlme)
library(joineRML)
library(survival)

data_dir <- "D:/AlterPC/Masterarbeit/Dateien/13_data_joinerml_testing/testing_data_joinerml"

file <- file.path(
  data_dir,
  "DIVAT_sim_missing_train_100_visit_1000_long_3_seed_1.csv"
)

dat <- read.csv(file)

# Create previous Y3 value
dat <- dat %>%
  arrange(id, visit) %>%
  group_by(id) %>%
  mutate(Y3_previous = lag(Y3)) %>%
  ungroup()

# Convert event indicator to 0/1
if (is.character(dat$event)) {
  dat$event <- ifelse(dat$event == "True", 1, 0)
}

if (is.logical(dat$event)) {
  dat$event <- as.integer(dat$event)
}

# Separate datasets for each longitudinal submodel
# Y1 requires Y1 and Y3_previous
dat_Y1 <- dat %>%
  dplyr::filter(
    !is.na(Y1),
    !is.na(Y3_previous)
  )

# Y2 requires Y2 and Y1
dat_Y2 <- dat %>%
  dplyr::filter(
    !is.na(Y2),
    !is.na(Y1)
  )

# Y3 requires Y3, Y1 and Y2
dat_Y3 <- dat %>%
  dplyr::filter(
    !is.na(Y3),
    !is.na(Y1),
    !is.na(Y2)
  )

# Keep only subjects with at least one usable observation
# for each longitudinal submodel
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

# Longitudinal datasets
dat_long <- list(
  "Y1" = dat_Y1,
  "Y2" = dat_Y2,
  "Y3" = dat_Y3
)

# Model specification
formLongFixed <- list(
  "Y1" = Y1 ~ obstime + Y3_previous + X1 + X2 + X3,
  "Y2" = Y2 ~ obstime + Y1 + X1 + X2 + X3,
  "Y3" = Y3 ~ obstime + Y1 + Y2 + X1 + X2 + X3
)

formLongRandom <- list(
  "Y1" = ~ obstime | id,
  "Y2" = ~ obstime | id,
  "Y3" = ~ 1 | id
)

formSurv <- Surv(time, event) ~ X1 + X2 + X3

# Fit joint model
model_mjoint <- mjoint(
  formLongFixed = formLongFixed,
  formLongRandom = formLongRandom,
  formSurv = formSurv,
  data = dat_long,
  timeVar = "obstime"
)

summary(model_mjoint)