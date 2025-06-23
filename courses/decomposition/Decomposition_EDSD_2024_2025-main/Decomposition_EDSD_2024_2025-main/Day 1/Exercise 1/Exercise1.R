# Setup -------------------------------------------------------------------
library(here)
wd <- here()
setwd(wd)

load("Day 1/Exercise 1/Kitagawa_Data.RData")

library(tidyverse)

# Extract the elements ----------------------------------------------------

# Select births for population 1
Bx1 <- data %>% 
  filter(pop=="CHL") %>% 
  pull(births)
# Select population for popuation 1
Nx1 <- data %>% 
  filter(pop=="CHL") %>% 
  pull(exposure)
# Do the same for population 2
Bx2 <- data%>% 
  filter(pop=="IRL") %>% 
  pull(births)
Nx2 <- data%>% 
  filter(pop=="IRL") %>% 
  pull(exposure)


# General fertility rates -------------------------------------------------------

# get the general fertility rate in population 1
GFR1 <- sum(Bx1)/sum(Nx1)
#general fertility expressed by 1000 in population 1
GFR1*1000
# the same for period 2
GFR2 <- sum(Bx2)/sum(Nx2)
#general fertility expressed by 1000 in population 2
GFR2*1000

#difference in GFR
(Diff <- (GFR2 - GFR1)*1000)

# Decomposition -----------------------------------------------------------

# population 1
# Select births for first population
# get age-specific fertility rates
ASFR1 <- Bx1/Nx1
# replace NA's with zero (just for simplicity)
ASFR1 <- ASFR1 %>% 
  replace_na(replace=0)
# do the same for the second population
ASFR2 <- Bx2/Nx2
ASFR2 <- ASFR2 %>% 
  replace_na(replace=0)

RC <- sum(0.5*(Nx2/sum(Nx2) + Nx1/sum(Nx1))*(ASFR2-ASFR1))
RC*1000

CC <- sum(0.5*(ASFR2+ASFR1)*(Nx2/sum(Nx2)-Nx1/sum(Nx1)))
CC*1000


# Check the results -------------------------------------------------------

RC*1000 + CC*1000

Diff

# Plot the result ---------------------------------------------------------

cbind(type="rate",effect=RC) %>% 
  rbind(cbind(type="composition",effect=CC)) %>%   
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes(x=1, y=as.numeric(effect), fill=type), stat="identity", position="stack") +
  scale_fill_manual(values=c(4,2))

