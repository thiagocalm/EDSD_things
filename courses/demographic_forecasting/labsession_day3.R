#'---------------------------------------
#'@date 2025-05-06
#'@program EDSD
#'@course Demographic forecasting
#'@professor Ugofillipo Basellini (MPIDR)
#'@description Course's lab sessions - day 3
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer


# libraries ---------------------------------------------------------------

library(pacman)
p_load(tidyverse, forecast, tseries)

# Importing data ----------------------------------------------------------

load("data/FertSWE.Rdata")


# Fitting fertility by age ------------------------------------------------

# data handle

# Filtering data
fert <- FERT.SWE |>
  filter(
    Year >= 1950
  )

# fertility 2000

fert_2000 <- FERT.SWE |>
  filter(
    Year == 2000
  )

# dataset

head(fert)

# dimensions of the problem
# t <- fert_2000$Year
age <- fert_2000$Age
age2 <- fert_2000$Age^2
y <- fert_2000$Births
e <- fert_2000$Exposures
tF <- min(t):2050 # creating the rage of years

## Model 1 - modeling as a GLM
# model: log-quadratic

mod1 <- glm(round(y) ~ age + age2, offset = log(e), family = poisson())

summary(mod1)

# predicting values
lF.GLM <- coef(mod1)[1] + coef(mod1)[2] * age + coef(mod1)[3] * age2

# plotting it...

ggplot() +
  aes(x = age, y = y/e) +
  geom_point(alpha = .5) +
  geom_line(aes(x = age, y = exp(lF.GLM), color = "orange3", linetype = "dashed", linewidth = 1.2))


# Extending it for age and time -------------------------------------------

# parameter for loop
time_serie <- unique(fert$Year)

for(i in seq_along(time_serie)){
  # fertility year t

  t <- time_serie[i]

  fert_t <- FERT.SWE |>
    filter(
      Year == t
    )

  # dimensions of the problem

  age <- fert_t$Age
  age2 <- fert_t$Age^2
  y <- fert_t$Births
  e <- fert_t$Exposures

  ## Model 1 - modeling as a GLM
  # model: log-quadratic

  mod1 <- glm(round(y) ~ age + age2, offset = log(e), family = poisson())

  summary(mod1)

  # predicting values
  lF.GLM <- coef(mod1)[1] + coef(mod1)[2] * age + coef(mod1)[3] * age2


  if(i == 1){
    df_pred <- tibble(
      year = t,
      age = age,
      pred_value = lF.GLM
    )
    df_coef <- tibble(
      year = t,
      beta_0 = coef(mod1)[1],
      beta_1 = coef(mod1)[2],
      beta_2 = coef(mod1)[3]
    )
  } else{
    df_pred <- df_pred |>
      bind_rows(
        tibble(
          year = t,
          age = age,
          pred_value = lF.GLM
        )
      )

    df_coef <- df_coef |>
      bind_rows(
        tibble(
          year = t,
          beta_0 = coef(mod1)[1],
          beta_1 = coef(mod1)[2],
          beta_2 = coef(mod1)[3]
        )
      )
  }
}

df_pred |>
  filter(year %in% c(1950, 1990, 2010, 2022)) |>
  ggplot() +
  aes(x = age, y = exp(pred_value), color = year, group = interaction(year,year)) +
  geom_line(linewidth = 1.2)


df_coef |>
  pivot_longer(beta_0:beta_2, values_to = "coef", names_to = "coef_name") |>
  ggplot() +
  aes(x = year, y = coef) +
  geom_line() +
  facet_wrap(.~coef_name,scales = "free_y")


# Forecasting coefficients using time-series methods ----------------------

# ACF for each coeff
par(mfrow=c(1,3))
Acf(df_coef$beta_0)
Acf(df_coef$beta_1)
Acf(df_coef$beta_2)
par(mfrow=c(1,1))

# ACF for each coeff - first-diff order
par(mfrow=c(1,3))
Acf(diff(df_coef$beta_0))
Acf(diff(df_coef$beta_1))
Acf(diff(df_coef$beta_2))
par(mfrow=c(1,1))

# tests
tseries::kpss.test(df_coef$beta_0)
tseries::kpss.test(df_coef$beta_1)
tseries::kpss.test(df_coef$beta_2)
# diff
tseries::kpss.test(diff(df_coef$beta_0))
tseries::kpss.test(diff(df_coef$beta_1))
tseries::kpss.test(diff(df_coef$beta_2))


## modeling it...

# AR - beta0

mod_beta0 <- auto.arima(df_coef$beta_0)
summary(mod_beta0)

# AR - beta1

mod_beta1 <- auto.arima(df_coef$beta_1)
summary(mod_beta1)

# AR - beta2
mod_beta2 <- auto.arima(df_coef$beta_2)
summary(mod_beta2)

## forecast the parameters
t_ends = 2050
tF = (max(df_coef$year)+1):2050
nF = length(tF)

beta0.F <- forecast(mod_beta0, h = nF)
plot(beta0.F)

beta1.F <- forecast(mod_beta1, h = nF)
plot(beta1.F)

beta2.F <- forecast(mod_beta2, h = nF)
plot(beta2.F)

## deriving forecast fertility pattern

ETA.fore <- matrix(NA,length(age),nF)

for(i in 1:nF){
  lfx.fore <- beta0.F$mean[i] + beta1.F$mean[i] * age + beta2.F$mean[i] * age2

  # saving it
  ETA.fore[,i] <- lfx.fore
}

colnames(ETA.fore) <- tF

## Plotting it...


# Generalized log-quadratic model for fertility LCI -----------------------

head(fert)

# dimensions of the problem
t <- fert$Year
age <- fert$Age
age2 <- fert$Age^2
y <- fert$Births
e <- fert$Exposures
t.unique <- unique(t)

## Model 1 - modeling as a GLM
# model: log-quadratic

mod_LC1 <- glm(round(y) ~ age + age2 + t, offset = log(e), family = poisson())

summary(mod_LC1)

# defining parameters
t_ends = 2050
tF.all = min(t):2050
nF.all = length(tF.all)

# time pattern - extrapolating
time.pattern <- coef(mod_LC1)[4] * tF.all

## deriving forecast fertility pattern

ETA.fore <- matrix(NA,length(unique(age)),nF.all)
i = 1
for(i in 1:nF.all){
  lfx.fore <- coef(mod_LC1)[1] + coef(mod_LC1)[2] * unique(age) + coef(mod_LC1)[3] * unique(age2) + time.pattern[i]

  # saving it
  ETA.fore[,i] <- lfx.fore

}

# naming columns
colnames(ETA.fore) <- tF.all

## Plotting it...

df_plot <- fert |>
  as_tibble() |>
  select("year" = Year, "age" = Age, logRates) |>
  mutate(
    type = "Observed"
  ) |>
  bind_rows(
    ETA.fore |>
      as_tibble() |>
      mutate(age = unique(age)) |>
      pivot_longer(
        glue::glue(min(tF.all)):glue::glue(max(tF.all)),
        names_to = "year",
        values_to = "logRates"
      ) |>
      select(year, age, logRates) |>
      mutate(
        year = as.integer(year),
        type = "Fitted"
      )
  )

# plotting log-ASFR

df_plot |>
  filter(!is.infinite(logRates)) |>
  ggplot() +
  aes(x = age, y = logRates, color = year, group = interaction(year, year)) +
  geom_line() +
  facet_wrap(.~type) +
  theme_light() +
  scale_color_viridis_c(option = "D")

# plotting TFR

df_tfr <- df_plot |>
  mutate(
    ASFR = case_when(is.infinite(logRates) ~ 0, TRUE ~ exp(logRates))
  ) |>
  reframe(
    TFR = sum(ASFR),
    .by = c(year, type)
  )

ggplot() +
  geom_line(
    aes(
      x = df_tfr$year[df_tfr$type == "Fitted"],
      y = df_tfr$TFR[df_tfr$type == "Fitted"],
      color = "Fitted"
    ),
    color = "orange2",
    linewidth = 1.2
  ) +
  geom_point(
    aes(
      x = df_tfr$year[df_tfr$type != "Fitted"],
      y = df_tfr$TFR[df_tfr$type != "Fitted"],
      color = "Observed"
    ),
    color = "black",
    size = 2
  ) +
  labs(
    x = "Age",
    y = "TFR"
  ) +
  theme_light()

## Simulating fitted values using bootstrap

## QUESTION: HOW DO WE SIMULATE A GLM???

# # number of simulations
# nS <- 100    ## increase for improved precision
# set.seed(1)  ## for reproducibility
# tF <- (max(fert$Year)+1):2050 # creating the rage of years
# # object to store simulations
# h = length(unique(age))
# LFsim <- matrix(NA,h,nS)
# s <- 1
#
# LFsim <- simulate(
#   mod_LC1,
#   nsim=nS,
#   future=TRUE,
#   bootstrap=TRUE
# )
#
# for (s in 1:nS){
#   lf.fore.sim <- simulate(
#     mod_LC1,
#     nsim=h,
#     future=TRUE,
#     bootstrap=TRUE
#   )
#   ## saving
#   LFsim[,s] <- lf.fore.sim
# }
#
# ## plotting all simulations
# plot(t,y,ylim=range(y,LFsim),xlim=range(t,tF))
# matlines(tF,LFsim,col="grey80",lty=1)
#
# ## deriving median and 95% PI
# lev <- 95
# lev.p <- lev/100
# lf.fore.med <- apply(LFsim,1,median) # taking the median based on the rows of a matrix
# lf.fore.low <- apply(LFsim,1,quantile,prob=(1-lev.p)/2) # taking the lower CI based on the rows of a matrix
# lf.fore.up <- apply(LFsim,1,quantile,prob=1-(1-lev.p)/2) # taking the upper CI based on the rows of a matrix
#
# ## comparing PIs
# plot(t,y,ylim=range(y,fF.AR$lower,lf.fore.low),xlim=range(t,tF))
# ## analytical
# lines(tF,fF.AR$mean,col=3,lwd=2)
# lines(tF,fF.AR$upper[,2],col=3,lwd=2,lty=2)
# lines(tF,fF.AR$lower[,2],col=3,lwd=2,lty=2)
# ## simulations
# lines(tF,lf.fore.med,col=4,lwd=2)
# lines(tF,lf.fore.up,col=4,lwd=2,lty=2)
# lines(tF,lf.fore.low,col=4,lwd=2,lty=2)
# legend("bottomleft",c("analytical","simulations"),col=c(3,4),lwd=2)
