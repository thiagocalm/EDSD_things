# Setup -------------------------------------------------------------------
library(here)
wd <- here()
setwd(wd)

load("Day 1/Exercise 1/NishikidoCuiEsteve_Kitagawa.RData")

library(tidyverse)


# Decomposition by partnership status and age -----------------------------

# Extract the elements ----------------------------------------------------

# Select births for Spain, for unpartnered and partnered women
bx0_ESP <- data %>% 
  filter(pop=="ESP", partnered==0) %>% 
  pull(births)
bx1_ESP <- data %>% 
  filter(pop=="ESP", partnered==1) %>% 
  pull(births)
# Select population for Spain, for unpartnered and partnered women
Nx0_ESP <- data %>%
  filter(pop=="ESP", partnered==0) %>% 
  pull(exposure)
Nx1_ESP <- data %>% 
  filter(pop=="ESP", partnered==1) %>% 
  pull(exposure)
# Do the same for Sweden
bx0_SWE <- data %>% 
  filter(pop=="SWE", partnered==0) %>% 
  pull(births)
bx1_SWE <- data %>% 
  filter(pop=="SWE", partnered==1) %>% 
  pull(births)
Nx0_SWE <- data %>% 
  filter(pop=="SWE", partnered==0) %>%
  pull(exposure)
Nx1_SWE <- data %>% 
  filter(pop=="SWE", partnered==1) %>% 
  pull(exposure)

# Difference in TFR -------------------------------------------------------

# Spain
TFR_ESP <- sum(bx1_ESP+bx0_ESP)/(Nx1_ESP+Nx0_ESP)[1]
# the same for Sweden
TFR_SWE <- sum(bx1_SWE+bx0_SWE)/(Nx1_SWE+Nx0_SWE)[1]
# difference in TFR
(diff <- TFR_SWE - TFR_ESP)

# Decomposition -----------------------------------------------------------

# Partnership-specific ASFR for Spain
ASFR0_ESP <- bx0_ESP/Nx0_ESP
ASFR1_ESP <- bx1_ESP/Nx1_ESP
# And Sweden
ASFR0_SWE <- bx0_SWE/Nx0_SWE
ASFR1_SWE <- bx1_SWE/Nx1_SWE

# Unpartnered women
RC0 <- 0.5*(Nx0_SWE/(Nx0_SWE+Nx1_SWE) + Nx0_ESP/(Nx0_ESP+Nx1_ESP))*(ASFR0_SWE-ASFR0_ESP)
CC0 <- 0.5*(ASFR0_SWE+ASFR0_ESP)*(Nx0_SWE/(Nx0_SWE+Nx1_SWE)-Nx0_ESP/(Nx0_ESP+Nx1_ESP))

# Partnered women
RC1 <- 0.5*(Nx1_SWE/(Nx0_SWE+Nx1_SWE) + Nx1_ESP/(Nx0_ESP+Nx1_ESP))*(ASFR1_SWE-ASFR1_ESP)
CC1 <- 0.5*(ASFR1_SWE+ASFR1_ESP)*(Nx1_SWE/(Nx0_SWE+Nx1_SWE)-Nx1_ESP/(Nx0_ESP+Nx1_ESP))

# Sum across partnership status (first summation)
RC_age <- RC0+RC1
CC_age <- CC0+CC1

# Sum across ages (second summation)
RC <- sum(RC_age)
CC <- sum(CC_age)

# Check the results -------------------------------------------------------
RC+CC
diff

(sum(RC0+CC0)+sum(RC1+CC1))

# Graph the results -------------------------------------------------------

cbind(type="rate",effect=sum(RC0+RC1)) %>% 
  rbind(cbind(type="composition",effect=sum(CC0+CC1))) %>%   
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes(x=1, y=as.numeric(effect), fill=type), stat="identity", position="stack") +
  scale_fill_manual(values=c(4,2))


cbind(partnership=0,type="rate",effect=sum(RC0)) %>% 
  rbind(cbind(partnership=1,type="rate",effect=sum(RC1))) %>% 
  rbind(cbind(partnership=0,type="composition",effect=sum(CC0))) %>%   
  rbind(cbind(partnership=1,type="composition",effect=sum(CC1))) %>%   
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes(x=partnership, y=as.numeric(effect), fill=type), stat="identity", position="stack") +
  scale_fill_manual(values=c(4,2))


cbind(age=18:40,type="rate",effect=RC_age) %>% 
  rbind(cbind(age=18:40,type="composition",effect=CC_age)) %>% 
  as.data.frame() %>% 
  ggplot() +
  geom_bar(aes(x=age, y=as.numeric(effect), fill=type), stat="identity", position="stack") +
  scale_fill_manual(values=c(4,2))
