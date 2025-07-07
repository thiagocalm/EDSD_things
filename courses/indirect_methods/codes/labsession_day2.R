rm(list = ls())
options(scipen = 999999)
# Packages ----------------------------------------------------------------

library(demogsurv)
library(rdhs)
library(tidyverse)

# Sibling mortality -------------------------------------------------------

# parameter for loop
country_2d <- "GU" # country
type_data <- "SSH"
dhs_years <- c(1995,2015)
# dhs_years <- c(2015)

for(i in seq_along(dhs_years)){
  # Import data
  load(glue::glue("data/GU/{type_data}{country_2d}{dhs_years[i]}DHS.rda"))

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
  nqx <- calc_nqx(sib, by=~mm1, agegr=seq(15, 60, 5), tips=c(0, 4, 8, 12), dob="mm4", dod="mm8", strata = NULL) |>
    select("sex" = mm1, everything()) |>
    mutate(
      sex = factor(sex, levels = c(1,2),labels = c("Female","Male"))
    )

  # period of reference

  avg <- mean(sib$v008 / 12 + 1900, na.rm = TRUE) #we convert the CMC into a date + take the mean date (based on days)

  # adding period of reference to dataset

  nqx <- nqx |>
    as_tibble() |>
    mutate(
      referecence_date = avg
    ) |>
    # transforming TIPS to years
    mutate(
      tips_midpoint = case_when(
        tips == "0-3" ~ 1.5,
        tips == "4-7" ~ 5.5,
        tips == "8-11" ~ 10.5
      ),
      reference_year = round(referecence_date - tips_midpoint,0),
      survey = paste0(type_data,country_2d,dhs_years[i])
    )

  # storing results
  if(!"df" %in% ls()){
    df <- nqx
  } else{
    df <- df |>
      bind_rows(nqx)
  }
  rm(nqx)
}




### Compare data with other datasources
# wpp2024 - we have to manipulate age-specific mortality rates to get 35q15
# WDI - world bank data - they use WPP data

# auxiliar table
country_table <- read.delim(
  file = "https://github.com/thiagocalm/tca_utils/raw/refs/heads/master/from-to%20tables/table_countries_uncodes.txt",
  header = TRUE,
  sep = ","
)

# alpha 2 digit codes
alpha2d <- country_table[country_table$Country == "Guatemala",]$Alpha2code

wpp_45q15 <- WDI::WDI(country = alpha2d, indicator = "SP.DYN.AMRT.MA") |>
  select(everything(),"nqx" = 5) |>
  filter(
    !is.na(nqx),
    year %in% 1980:2015
  ) |>
  mutate(
    sex = "Female"
  ) |>
  bind_rows(
    WDI::WDI(country = alpha2d, indicator = "SP.DYN.AMRT.FE") |>
      select(everything(),"nqx" = 5) |>
      filter(
        !is.na(nqx),
        year %in% 1980:2015
      ) |>
      mutate(
        sex = "Male"
      )
  ) |>
  mutate(survey = "WPP 2024 Reivew") |>
  select(survey,year,sex, nqx)


# joining datasets

df <- df |>
  select(survey,"year" = reference_year,sex, "nqx" = est, ci_l, ci_u) |>
  mutate(sex = as.character(sex)) |>
  mutate(
    nqx = nqx * 1000,
    ci_l = ci_l * 1000,
    ci_u = ci_u * 1000
  ) |>
  bind_rows(wpp_45q15)

# plot

ggplot(df, aes(x = year, y = nqx)) +
  geom_ribbon(aes(ymin = ci_l, ymax = ci_u, group = interaction(survey,sex)), fill = "grey88", alpha = 0.1, color = NA, show.legend = FALSE) +
  geom_line(aes(color = survey, linetype = sex), linewidth = 0.8) +
  geom_point(aes(color = survey), size = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = bquote('Adult mortality: '~{""[45]} * q[15]~' by sex in Guatemala' ),
    x = "Reference Year",
    y = bquote(~{""[45]} * q[15]~' per 1000'),
    color = "Data Source",
    linetype = "",
    fill = "Data Source",
    caption = "Data: DHS and UN/DESA, WPP 2024 Revision."
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

