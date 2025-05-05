#'---------------------------------------
#'@date 2025-04-30
#'@program EDSD
#'@course Demographic forecasting
#'@professor Ugofillipo Basellini (MPIDR)
#'@description Course's lab sessions - day 1
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer


# libraries ---------------------------------------------------------------

library(tidyverse)


# Importing data ----------------------------------------------------------

load("data/FertSWE.Rdata")

# Model 1 - linear model --------------------------------------------------
head(FERT.SWE)

# Filtering data
fert_20 <- FERT.SWE |>
  filter(
    Year >= 1950,
    Age == 20
  )

mod1 <- lm(Births ~ Year, data = fert_20)

summary(mod1)

## predicting values and extrapolating it until 2050

# parameters
time = seq(1950,2050,1)
beta1_hat = mod1$coefficients[2]
beta0_hat = mod1$coefficients[1]

# dataframe with predictions
predicted <- data.frame(
  Years = time,
  pred_births = beta0_hat + beta1_hat * time
) |>
  mutate(
    type = case_when(Years <= 2022 ~ "Predicted", TRUE ~ "Projected")
  )

# plotting it...

predicted |>
  ggplot() +
  aes(x = Years, y = pred_births, linetype = type) +
  geom_line(linewidth = 1.3) +
  geom_point(aes(x = fert_20$Year, y = fert_20$Births)) +
  theme_minimal()

## Ugo's approach for forecasting

# dimensions of the problem
t <- fert_20$Year
y <- fert_20$Births

# fit and focast of the model
tF <- min(t):2050 # creating the rage of years

df_forecast <- tibble(Year = tF) # creating df with rage of years

# forecasting years - Option 1

yF <- predict(object = mod1, newdata = df_forecast)

# Model 2 - modeling rates with poisson distribution ----------------------

# dataset

head(fert_20)

# dimensions of the problem
t <- fert_20$Year
y <- fert_20$Rates
tF <- min(t):2050 # creating the rage of years

## Model 2.1 - modeling linearly the rates

mod2.1 <- lm(y ~ t)

summary(mod2.1)

# forecasting with lm
fF.LM <- coef(mod2.1)[1] + coef(mod2.1)[2] * tF

## Model 2.2 - modeling via Poisson taking into account offset

# dimensions of the problem
t <- fert_20$Year
y <- fert_20$Births
e <- fert_20$Exposures
tF <- min(t):2050 # creating the rage of years

mod2.2 <- glm(y ~ Year, data = fert_20, offset = log(e), family = poisson())
# Warnings are because of the non integer values for births... to avoid that:
mod2.2 <- glm(round(y) ~ Year, data = fert_20, offset = log(e), family = poisson())

summary(mod2.2)

# forecasting with glm
fF.GLM <- exp(coef(mod2.2)[1] + coef(mod2.2)[2] * tF)

# plotting it...

mod2_fit <- tibble(
  Years = tF,
  rates_linear = fF.LM,
  rates_poisson = fF.LM,
) |>
  mutate(
    type = case_when(Years <= 2022 ~ "Predicted", TRUE ~ "Projected")
  )

# mod2_fit |>
#   ggplot() +
#   aes(x = Years, y = pred_births, linetype = type) +
#   geom_line(linewidth = 1.3) +
#   geom_point(aes(x = fert_20$Year, y = fert_20$Births)) +
#   theme_minimal()
