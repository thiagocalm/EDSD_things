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

## IMPROVE IT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

for(i in 2:length(dhs_years)){
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

  # rename it...

  brfile <- BH

  # way 1
  avg <- mean(brfile$v008 / 12 + 1900, na.rm = TRUE) #we convert the CMC into a date + take the mean date (based on days)

  # Way 2
  # avg <- sum(as.numeric(lubridate::ymd(dhs_reference$FieldworkStart)),as.numeric(lubridate::ymd(dhs_reference$FieldworkEnd)))/2
  #
  # avg <- lubridate::as_date(avg)
  #
  # avg <- lubridate::year(avg)

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

  # creating variables to run the package in case there is no availability of them
  if(!"v021" %in% names(brfile)){
    brfile <- brfile |>
      mutate(v021 = 1)
  }
  if(!"v024" %in% names(brfile)){
    brfile <- brfile |>
      mutate(v024 = 1)
  }
  if(!"v025" %in% names(brfile)){
    brfile <- brfile |>
      mutate(v025 = 1)
  }

  # store it in a survey

  nqx <- calc_nqx(
    brfile,
    agegr=c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12
  ) |>
    as_tibble() |>
    mutate(
      referecence_date = avg
    )

  # adjust the tips to be consider the year since survey

  nqx <- nqx |>
    mutate(
      tips_midpoint = case_when(
        tips == "0-4" ~ 2.5,
        tips == "5-9" ~ 7.5,
        tips == "10-14" ~ 12.5
    ),
    reference_year = round(referecence_date - tips_midpoint,0),
    survey = paste0(type_data,country_2d,dhs_years[i])
  )

    if(!"df" %in% ls()){
      df <- nqx
    } else{
      df <- df |>
        bind_rows(nqx)
    }
  rm(nqx)
}

ggplot(df, aes(x = reference_year, y = est, group = survey)) +
  geom_ribbon(aes(ymin = ci_l, ymax = ci_u, fill = survey), alpha = 0.2, color = NA) +
  geom_line(aes(color = survey), linewidth = 0.8) +
  geom_point(aes(color = survey), size = 2) #+
  # geom_ribbon(data = GU_U5_UNIG, aes(x = year, ymin = ci_l, ymax = ci_u, fill = survey),
  #             alpha = 0.2, color = NA) +
  # geom_line(data = GU_U5_UNIG, aes(x = year, y = est, color = survey), linewidth = 0.5) +
  # scale_color_manual(values = c("UN IGME" = "blue",
  #                               setNames(rainbow(length(unique(nqx_combined$survey))),
  #                                        unique(nqx_combined$survey)))) +
  # scale_fill_manual(values = c("UN IGME" = "lightblue",
  #                              setNames(rainbow(length(unique(nqx_combined$survey)), alpha = 0.3),
  #                                       unique(nqx_combined$survey)))) +
  # labs(title = "Under-5 Mortality Rate in Guatemala",
  #      x = "Reference Year",
  #      y = "5q0 per 1000",
  #      color = "Data Source",
  #      fill = "Data Source",
  #      caption = "Data: DHS and UN IGME") +
  # theme_bw() +
  # theme(legend.position = "bottom")


# Comparing this estimates with WDI package!!!!!
