#'---------------------------------------
#'@date 2025-05-07
#'@program EDSD
#'@course Demographic forecasting
#'@professor Ugofillipo Basellini (MPIDR)
#'@description Course's lab sessions - day 4
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer


# libraries ---------------------------------------------------------------

library(pacman)
p_load(tidyverse, forecast, tseries,viridis, fields)

# Importing data ----------------------------------------------------------

load("data/MortSWE.Rdata")

# Data handling -----------------------------------------------------------

# replacing death cell with 0 to 1

head(MORT.SWE)

MORT.SWE <- MORT.SWE |>
  mutate(
    Deaths = case_when(Deaths == 0 ~ 1, TRUE ~ Deaths)
  ) |>
  mutate(
    Rates = Deaths/Exposures,
    logRates = log(Rates)
  )

# filtering data

mort <- MORT.SWE |>
  filter(
    Year >= 1950,
    Sex == "Male",
    Age %in% 0:100
  )


# LC model ----------------------------------------------------------------

## Estimating alpha parameter

alpha <- mort |>
  reframe(
    alpha = mean(logRates, na.rm = TRUE),
    .by = Age
  )

# Plotting it...

ggplot() +
  geom_point(data = mort, aes(x = Age, y = logRates, color = Year), size = 2, alpha = .1) +
  geom_line(data = alpha, aes(x = Age, y = alpha), color = "black", linewidth = 2) +
  theme_light() +
  scale_color_viridis_c(option = "C")

## Ugo's approach

# dimensions of the problem
x <- unique(mort$Age)
t <- unique(mort$Year)
m <- length(x)
n <- length(t)

## extract matrices from dataset
Y <- matrix(mort$Deaths, m, n)
E <- matrix(mort$Exposures, m, n)
MX <- Y/E
LMX <- log(MX)
image(t, x, t(LMX), col = viridis(n))

## Alpha
Alpha <- apply(LMX, 1, mean)

## Centering values (LMX - Alpha)

LMX_centered <- LMX - Alpha
image.plot(t,x,t(LMX_centered), col=viridis(n))

## Applying SVD for this centered matrix

M <- svd(LMX_centered)

# extracting parameters

Beta <- M$u[,1]

Kappa <- M$v[,1]

## plotting it...

par(mfrow=c(1,3))
plot(x,Alpha,type = "l")
plot(x,Beta,type = "l")
plot(t,Kappa,type = "l")
par(mfrow=c(1,1))

## including constraints
# constraint 1
sum.Beta <- sum(Beta)
Beta <- Beta/sum.Beta
sum(Beta)
# constraint 2
Kappa1 <- M$d[1] * Kappa * sum.Beta
sum(Kappa1)
# plotting it
par(mfrow=c(1,3))
plot(x,Alpha,type = "l")
plot(x,Beta,type = "l")
plot(t,Kappa,type = "l")
par(mfrow=c(1,1))
