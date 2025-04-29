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

## Adding disaggregation by each component - balance equation

# function to project population

proj_ConstantRates_byComponent <- function(N0, b, d, m, time){
  r <- b - d + m
  NT <- N0 * exp(r * time)
}

# calculating new rates
# pop a
ba = .03
da = .05
ma = .01

# pop b
bb = .07
db = .04
mb = .03

# projecting population

na_proj <- proj_ConstantRates_byComponent(na_0, ba,da,ma, time)
nb_proj <- proj_ConstantRates_byComponent(nb_0, bb,db,mb, time)

# total pop at time t = 10y

na_proj[length(na_proj)]
nb_proj[length(nb_proj)]

# pop b surpass pop a
that <- time[which(nb_proj > na_proj)][1]

# plotting it

ggplot() +
  aes(x = time) +
  geom_line(aes(x = time, y = na_proj), color = "green4", linewidth = 1.2) +
  geom_line(aes(x = time, y = nb_proj), color = "black", linewidth = 1.2) +
  geom_vline(xintercept = that, color = "red", linetype = "dashed",linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_y_continuous(breaks = seq(30,max(nb_proj)+5,10)) +
  theme_minimal()

## adding Rogers' suggestion for migration

# calculating new rates
time = seq(0,20,1)
n0 = 50
b = .06
d = .04
e = .01
I = 1.5

# function to project population
proj_ConstantRates_byComponent_Rogers <- function(N0, b, d, e, I, time){
  r <- b - d - e
  NT = rep(NA,length(time))
  NT[1] = N0
  for(t in 2:length(time)){
    NT[t] <-  NT[t-1] * exp(r) + I
  }
  return(NT)
}

# projecting population
n_proj = NULL
n_proj = proj_ConstantRates_byComponent_Rogers(n0, b, d, e, I, time)

# total pop at time t = 20y

n_proj[length(n_proj)]

## adding rates varying over time

# calculating new rates
time = seq(0,20,1)
n0 = 50
b0 = .06
d0 = .04
e0 = .01
e2 = c(rep(e,11),rep(e*2,10))
I0 = 1.5
I2 = c(rep(I0,11),seq(I0,0,length.out = 10))

# function to project population based on different assumptions
proj_ConstantRates_byComponent_Rogers_changing_time <- function(N0, b, d, e, I, time){
  NT = rep(NA,length(time))
  NT[1] = N0
  for(t in 2:length(time)){
    r <- b - d - e[t]
    NT[t] <-  NT[t-1] * exp(r) + I[t]
  }
  return(NT)
}

# projecting population - REVIEW THAT
n_proj_scenario1 = proj_ConstantRates_byComponent_Rogers(n0, b0, d0, e0, I0, time)
n_proj_scenario2 = proj_ConstantRates_byComponent_Rogers(n0, b0, d0, e2, I2, time)

# total pop at time t = 20y

n_proj[length(n_proj)]


# Model 2 - cohort-component method ---------------------------------------

# importing data
load("data/dta.swe.1993.Rdata")

head(dta.swe)

# Exercise 1 - projecting female population


# CCM function - for female... we have to generalize it

function_ProjPop_CCM <- function(N0,LFx, time, age, SRB, l0, Fx){
  # Calculating the Sx
  leaded = lead(LFx)
  SFx = leaded/LFx
  SFx[length(SFx)-1] = leaded[length(SFx)-1]/(leaded[length(SFx)-2]+leaded[length(SFx)-1])
  Sx = as.matrix(Sx)
  # Computing the population projection for each age group
  NT <- matrix(NA,nrow = length(N0), ncol = length(time))
  NT[,1] <- N0
  for(t in 2:length(time)){
    # Creating births
    bf = (1)/(1 + SRB) * (LFx)/(2 * l0) * (Fx + Sx * lead(Fx))
    bf[is.na(bf)] = 0
    # all the groups
    NT[,t] = NT[,t-1] * Sx[,t-1]
    # Opened-age group
    NT[length(age),t] = (NT[length(age)-1,t-1] + NT[length(age),t-1]) * Sx[length(age)-1,t-1]
    # First age group
    NT[1,t] = sum(NT[,t-1] * bf)
  }
  return(NT)
}

N_female_proj <- function_ProjPop_CCM(
  N0 = dta.swe$NFx,
  LFx = dta.swe$LFx,
  time = 0:1,
  age = dta.swe$Age,
  SRB = 1.05,
  l0 = 100000,
  Fx = dta.swe$Fx
)
