library(dplyr)
library(nlme)

data_dir <- "D:/AlterPC/Masterarbeit/Dateien/13_master_data_joinerml/data_joinerml_train"

file <- file.path(
  data_dir,
  "DIVAT_sim_missing_train_100_visit_1000_long_3_seed_1.csv"
)

dat <- read.csv(file)

# Previous value of Y3
dat <- dat %>%
  arrange(id, visit) %>%
  group_by(id) %>%
  mutate(Y3_previous = lag(Y3)) %>%
  ungroup()


# Y1: Simple vs. + Y3_previous
# Same observations for both models
dat_Y1 <- dat %>%
  filter(
    !is.na(Y1),
    !is.na(Y3_previous)
  )

fit_Y1_simple <- lme(
  Y1 ~ obstime + X1 + X2 + X3,
  random = ~ obstime | id,
  data = dat_Y1
)

fit_Y1_extended <- lme(
  Y1 ~ obstime + Y3_previous + X1 + X2 + X3,
  random = ~ obstime | id,
  data = dat_Y1
)

print(VarCorr(fit_Y1_simple))
print(VarCorr(fit_Y1_extended))


# Y2: Simple vs. + Y1
# Same observations for both models
dat_Y2 <- dat %>%
  filter(
    !is.na(Y2),
    !is.na(Y1)
  )

fit_Y2_simple <- lme(
  Y2 ~ obstime + X1 + X2 + X3,
  random = ~ obstime | id,
  data = dat_Y2
)

fit_Y2_extended <- lme(
  Y2 ~ obstime + Y1 + X1 + X2 + X3,
  random = ~ obstime | id,
  data = dat_Y2
)

print(VarCorr(fit_Y2_simple))
print(VarCorr(fit_Y2_extended))


# Y3: Simple vs. + Y2
# Same observations for both models
dat_Y3 <- dat %>%
  filter(
    !is.na(Y3),
    !is.na(Y2)
  )

fit_Y3_simple <- lme(
  Y3 ~ obstime + X1 + X2 + X3,
  random = ~ 1 | id,
  data = dat_Y3
)

fit_Y3_extended <- lme(
  Y3 ~ obstime + Y2 + X1 + X2 + X3,
  random = ~ 1 | id,
  data = dat_Y3
)

print(VarCorr(fit_Y3_simple))
print(VarCorr(fit_Y3_extended))
