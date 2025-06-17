options(scipen = 999999)
# Packages ----------------------------------------------------------------

library(demogsurv)
library(rdhs)
library(tidyverse)

# Working with modelling data from package --------------------------------

# Importing DHS data for modelling

data("zzbr") # model data

# selecting variables

brfile <- zzbr[grep("caseid|^v0|^v1|^b", names(zzbr))]

# define outcome and date of death

brfile$death =  brfile$b5 == "no"
brfile$dod = NA
brfile$unit = trunc(brfile$b6/100); brfile$value = brfile$b6 - brfile$unit*100

# Unit == 1 means the age at death was measured in days : we add half a day
brfile$dod[!is.na(brfile$unit) & brfile$unit == 1] = brfile$b3[!is.na(brfile$unit)
                                                               & brfile$unit == 1] + brfile$value[!is.na(brfile$unit) & brfile$unit == 1]/28 + 0.5/28
# For unit = 2 (months) and deaths below age 1: add half a month
brfile$dod[!is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24 & brfile$unit == 2 ] =
  brfile$b3[!is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24 & brfile$unit == 2 ] +
  brfile$b7[!is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24 & brfile$unit == 2 ] + 0.5
brfile$dod[is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24 ] =
  brfile$b3[is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24  ] +
  brfile$b7[is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24  ] + 0.5

# For deaths above age 2, trunc B7 to the year, and reallocate a date of death
brfile$dod[brfile$death == TRUE & brfile$b7 >= 24] = brfile$b3[brfile$death == TRUE & brfile$b7 >= 24] + trunc(brfile$b7[brfile$death == TRUE & brfile$b7 >= 24]/12)*12+ 6

# Calculate mortality by age
calc_nqx(brfile, agegr=c(0, 1)/12)

# Under-5 mortality
calc_nqx(brfile, agegr=c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12)

# Under-5 mortality by sex
brfile$sex = brfile$b4 # factor(, levels = 1:2, labels = c("m", "f"))
calc_nqx(brfile, agegr=c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12, by= ~ sex)

# Working with Guatemala --------------------------------------------------

# parameter for loop
country_2d <- "GU" # country
type_data <- "BH"
dhs_years <- c(1987,1995,1999,2015)
i = 1

## IMPROVE IT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

for(i in seq_along(dhs_years)){
  # import data
  load(file.path("data/GU",paste0(type_data,country_2d,dhs_years[i],"DHS.rda")))

  # parameter for dhs period of reference
  dhs_reference <- dhs_surveys() |>
    as_tibble() |>
    filter(SurveyId %in% paste0(country_2d,dhs_years[i],"DHS"))
    # assume the mid period for the Time previous surveys (TIPS)
    # calculate the mid point of the data collection
    # way 1 - take the mean date of data collection V008
    # way 2 - take the mean of the difference between data start and end


    if(i == 1){
      df <- nqx
    } else{
      df <- df |>
        bind_rows(nqx)
    }
}

# Importing DHS data for modelling

load("data/GU/BHGU2015DHS.rda")

# rename it...

brfile <- BH

# selecting variables

brfile <- brfile[grep("caseid|^v0|^v1|^b", names(brfile))]

# define outcome and date of death

brfile$death =  brfile$b5 == 0
brfile$dod = NA
brfile$unit = trunc(brfile$b6/100); brfile$value = brfile$b6 - brfile$unit*100

# Unit == 1 means the age at death was measured in days : we add half a day
brfile$dod[!is.na(brfile$unit) & brfile$unit == 1] = brfile$b3[!is.na(brfile$unit)
                                                               & brfile$unit == 1] + brfile$value[!is.na(brfile$unit) & brfile$unit == 1]/28 + 0.5/28
# For unit = 2 (months) and deaths below age 1: add half a month
brfile$dod[!is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24 & brfile$unit == 2 ] =
  brfile$b3[!is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24 & brfile$unit == 2 ] +
  brfile$b7[!is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24 & brfile$unit == 2 ] + 0.5

brfile$dod[is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24 ] =
  brfile$b3[is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24  ] +
  brfile$b7[is.na(brfile$unit) & brfile$death == TRUE & brfile$b7 < 24  ] + 0.5

# For deaths above age 2, trunc B7 to the year, and reallocate a date of death
brfile$dod[brfile$death == TRUE & brfile$b7 >= 24] = brfile$b3[brfile$death == TRUE & brfile$b7 >= 24] + trunc(brfile$b7[brfile$death == TRUE & brfile$b7 >= 24]/12)*12+ 6

# Under-5 mortality
calc_nqx(brfile, agegr=c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12)

# store it in a survey

calc_nqx(brfile, agegr=c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12) |>
  as_tibble() |>
  mutate(year_dhs = year)


# Comparing this estimates with WDI package!!!!!
