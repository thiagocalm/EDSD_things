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
load("data/FertSWE.Rdata")

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

# Autoarima comparing fertility rates -------------------------------------

# Filtering data
fert_20 <- FERT.SWE |>
  filter(
    Year >= 1950,
    Age == 20
  )

## GLM

# dimensions of the problem
t <- fert_20$Year
y <- fert_20$Births
e <- fert_20$Exposures
tF <- min(t):2050 # creating the rage of years

mod_GLM <- glm(round(y) ~ Year, data = fert_20, offset = log(e), family = poisson())

# forecasting with glm
fF.GLM <- exp(coef(mod_GLM)[1] + coef(mod_GLM)[2] * tF)

## ARIMA

# dimensions of the problem
t <- fert_20$Year
y <- fert_20$logRates
tF <- min(t):2050 # creating the rage of years

# plotting it...

ggplot() +
  aes(x = t, y = y) +
  geom_point()

# ACF
Acf(y)
# PACF
Acf(y,type = "partial")

# taking the first-order diff...
y.diff <- diff(y)

# ACF
Acf(y.diff)
# PACF
Acf(y.diff,type = "partial")

# tests
tseries::kpss.test(y)
tseries::kpss.test(y.diff)


## modeling it...

# RWD

mod2_RWD <- Arima(
  y = y,
  order = c(0,1,0),
  include.drift = TRUE
)

summary(mod2_RWD)

# AR

mod2_ar <- auto.arima(y)
summary(mod2_ar)

## comparing forecasts
t_ends = 2050
tF = t_ends - max(t)

fF.RWD <- forecast(mod2_RWD, h = tF)
plot(fF.RWD)

fF.AR <- forecast(mod2_ar, h = tF)
plot(fF.AR)

## Simulating fitted values using bootstrap

# number of simulations
nS <- 100    ## increase for improved precision
set.seed(1)  ## for reproducibility
tF <- (max(fert_20$Year)+1):2050 # creating the rage of years
# object to store simulations
s <- 1
h = length(tF)
LFsim <- matrix(NA,h,nS)
for (s in 1:nS){
  lf.fore.sim <- simulate(
    mod2_ar,
    nsim=h,
    future=TRUE,
    bootstrap=TRUE
  )
  ## saving
  LFsim[,s] <- lf.fore.sim
}

## plotting all simulations
plot(t,y,ylim=range(y,LFsim),xlim=range(t,tF))
matlines(tF,LFsim,col="grey80",lty=1)

## deriving median and 95% PI
lev <- 95
lev.p <- lev/100
lf.fore.med <- apply(LFsim,1,median) # taking the median based on the rows of a matrix
lf.fore.low <- apply(LFsim,1,quantile,prob=(1-lev.p)/2) # taking the lower CI based on the rows of a matrix
lf.fore.up <- apply(LFsim,1,quantile,prob=1-(1-lev.p)/2) # taking the upper CI based on the rows of a matrix

## comparing PIs
plot(t,y,ylim=range(y,fF.AR$lower,lf.fore.low),xlim=range(t,tF))
## analytical
lines(tF,fF.AR$mean,col=3,lwd=2)
lines(tF,fF.AR$upper[,2],col=3,lwd=2,lty=2)
lines(tF,fF.AR$lower[,2],col=3,lwd=2,lty=2)
## simulations
lines(tF,lf.fore.med,col=4,lwd=2)
lines(tF,lf.fore.up,col=4,lwd=2,lty=2)
lines(tF,lf.fore.low,col=4,lwd=2,lty=2)
legend("bottomleft",c("analytical","simulations"),col=c(3,4),lwd=2)
