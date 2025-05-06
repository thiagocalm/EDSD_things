#'---------------------------------------
#'@date 2025-05-06
#'@program EDSD
#'@course Demographic forecasting
#'@professor Ugofillipo Basellini (MPIDR)
#'@description Course's lab sessions - day 2
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer


# libraries ---------------------------------------------------------------

library(pacman)
p_load(tidyverse, forecast, tseries)

# Importing data ----------------------------------------------------------

load("data/TimeSeries.Rdata")

# describing data ---------------------------------------------------------

head(sp500.data)

# data wrangling

df <- sp500.data |>
  filter(Date >= lubridate::as_date("1990-01-01")) |>
  slice(1:250)

# data for modelling

df_200 <- df |>
  slice(1:200)

# plotting it...

# acf

acf(df_200$SP500)

## checking if first-order difference solve the non-stationarity

y.diff = diff(df_200$SP500)

# plotting it...

acf(y.diff)

# testing for KPSS
# H0 is stationary
tseries::kpss.test(y.diff) # we can see that after first difference, the serie is stationary
tseries::kpss.test(df_200$SP500) # we can see that before first difference, the serie is non-stationary

# testing for ADF test
# H0 is non-stationary
tseries::adf.test(df_200$SP500)
tseries::adf.test(y.diff)

## fitting the models

# RW
mod_RW <- Arima(
  y = df_200$SP500, # y_t
  order = c(0,1,0), # applying first difference in the original variable
  include.drift = FALSE # model without drift
)

# RWD

mod_RWD <- Arima(
  y = df_200$SP500, # y_t
  order = c(0,1,0), # applying first difference in the original variable
  include.drift = TRUE # model without drift
)

## forecasting...

# RW

f.RW <- forecast(mod_RW, h = 50)
plot(f.RW)

# RWD

f.RWD <- forecast(mod_RWD, h = 50)
plot(f.RWD)

## plotting everything
tF <- df$Date[!df$Date %in%df_200$Date]# forecast period
plot(df$Date,df$SP500,ylim = range(df$SP500,f.RWD$mean))
points(df$Date,pch=16)
lines(tF,f.RW$mean,col = 2, lwd = 2)
lines(tF,f.RWD$mean,col = 4, lwd = 2)
