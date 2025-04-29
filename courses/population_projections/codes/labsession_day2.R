#'---------------------------------------
#'@date 2025-04-29
#'@program EDSD
#'@course Population Projections
#'@professor Marilia Nepomuceno (MPIDR)
#'@description Course's exercises - day 2
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer


# libraries ---------------------------------------------------------------

library(tidyverse)

# Model 1 - constant growth rate ------------------------------------------

# setting parameters

time <- seq(0,10,.01)
ra = 0.05
rb = 0.15
na_0 = 50
nb_0 = 35

# function to project population

proj_ConstantRates <- function(N0, r, time){
  NT <- N0 * exp(r * time)
}

# projecting

na_proj <- proj_ConstantRates(na_0, ra, time)
nb_proj <- proj_ConstantRates(nb_0, rb, time)

# total pop at time t = 10y

na_proj[length(na_proj)]
nb_proj[length(nb_proj)]

# pop b surpass pop a
t[which(nb_proj > na_proj)][1]


# plotting it

ggplot() +
  aes(x = time) +
  geom_line(aes(x = time, y = na_proj), color = "green4", linewidth = 1.2) +
  geom_line(aes(x = time, y = nb_proj), color = "black", linetype = "dashed", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_y_continuous(breaks = seq(30,max(nb_proj)+5,10)) +
  theme_minimal()

## Adding disaggregation by each component

# calculating new rates
# pop a
ba = .03
da = .05
ma = .01
ra = ba - da + ma

# pop b
bb = .07
db = .04
mb = .03
rb = bb - db + mb

# projecting population

na_proj <- proj_ConstantRates(na_0, ra, time)
nb_proj <- proj_ConstantRates(nb_0, rb, time)

# total pop at time t = 10y

na_proj[length(na_proj)]
nb_proj[length(nb_proj)]

# plotting it

ggplot() +
  aes(x = time) +
  geom_line(aes(x = time, y = na_proj), color = "green4", linewidth = 1.2) +
  geom_line(aes(x = time, y = nb_proj), color = "black", linetype = "dashed", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_y_continuous(breaks = seq(30,max(nb_proj)+5,10)) +
  theme_minimal()
