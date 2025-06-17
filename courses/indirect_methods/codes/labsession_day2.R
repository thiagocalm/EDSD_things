options(scipen = 999999)
# Packages ----------------------------------------------------------------

library(demogsurv)
library(rdhs)
library(tidyverse)


# Sibling mortality -------------------------------------------------------

# Import data
load("data/GU/SSHGU2015DHS.rda")

# rename it
irfile <- SSH

# data handling
irfile$v021 = labelled::unlabelled(irfile$v021)
irfile$v023 = labelled::unlabelled(irfile$v023)

# reshaping dataset
sib <- irfile[grep("caseid|^v0|^v1|^b|^mm", names(irfile))]
sib = reshape_sib_data(sib, widevars = c("v005", "v008", "v021", "v013", "v024"))
sib = sib[sib$mm2 %in% c(1,0) & sib$mm1 %in% c(1,2),]
sib$death <- sib$mm2 == 0; sib$mm8 <- sib$mm8 + 0.5
sib$mm1 = as.factor(sib$mm1)

# estimating it
calc_nqx(sib, by=~mm1, agegr=seq(15, 50, 5), tips=c(0, 4, 8, 12), dob="mm4", dod="mm8", strata = NULL)

# Compare data with other datasources
# wpp2024 - we have to manipulate age-specific mortality rates to get 35q15
# WDI - world bank data - they use WPP data
WDI::WDI(country = "GUA", indicator = "SP.DYN.AMRT.FE")
