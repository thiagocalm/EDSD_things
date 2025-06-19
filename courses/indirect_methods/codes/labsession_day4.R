options(scipen = 999999)
rm(list = ls())
invisible(gc())
# Packages ----------------------------------------------------------------

library(pacman)
p_load(demogsurv,tidyverse,DHS.rates,haven, ipumsr,wpp2024)

# Importing data ----------------------------------------------------------

ddi <- read_ipums_ddi(file.path("data","ipumsi_00037.xml"))
data <- read_ipums_micro(ddi)

# auxiliar datasets -------------------------------------------------------

data("age5categories")

# direct estiamtes -----------------------------------------------------------

census_dates <- c("2002-11-24","1994-04-17","1981-03-23","1973-03-26")

for(i in seq_along(census_dates)){
  # setting parameters
  year <- lubridate::year(lubridate::ymd(census_dates[i]))
  census_month <- lubridate::month(lubridate::ymd(census_dates[i]))

  # data handling
  df <- data %>%
    filter(YEAR == year & SEX ==2 & AGE2>3 & AGE2 < 18) %>%
    mutate(birth = if_else(LASTBYR == year,1,0))

  # Parse date with lubridate
  census_date <- lubridate::ymd(census_dates[i]) # census date from IPUMS
  ref_date <- ymd(paste0(year,"-01-01")) # reference date

  # Compute days since Jan 1, of each year - and then the faction of the year (exposure)
  days_since <- as.numeric(census_date - ref_date)
  fract_year <- days_since/365

  # compute the asfr

  asfr <- df %>%
    group_by(AGE2) %>%
    summarise(asfr=mean(birth/fract_year))

  # compute the TFR

  if(i == 1){
    tfr <- asfr |>
      summarise(
        year = year,
        tfr = 5 * sum(asfr),
        survey = paste0("census_",year)
      )
  } else{
    tfr <- tfr |>
      bind_rows(
        asfr |>
          summarise(
            year = year,
            tfr = 5 * sum(asfr),
            survey = paste0("census_",year)
          )
      )
  }
}

# Putting everything together ---------------------------------------------
load(file = file.path("courses","indirect_methods","data","tfr10.RData"))
load(file = file.path("courses","indirect_methods","data","tfr3.RData"))

# plotting it -------------------------------------------------------------

tfr10 |>
  bind_rows(tfr) |>
  ggplot() +
  aes(x = year, y = tfr, color = survey) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.0) +
  geom_line(data = tfr3, color = "black",linewidth = 1.1) +
  geom_point(data = tfr3, color = "black", size = 4) +
  geom_smooth(mapping=aes(group=1), se=FALSE, colour='red',,linewidth = 1.3) +
  scale_x_continuous(breaks = seq(min(tfr10$year)-5,max(tfr10$year)+2,5)) +
  labs(
    title = "Trends in TFR - Guatemala",
    caption = "Source: DHS. IPUMS, Demographic Census, 1973, 1981, 1994, 2002.",
    x = "Calendar year",
    y = "TFR"
  ) +
  theme_bw(base_size = 12)

# saving plot

ggsave(
  filename = "courses/indirect_methods/results/tfr_guatemala.jpeg",
  device = "jpeg",
  width = 10,
  height = 6.5
)


# Inputs for Reversal Estimates -------------------------------------------

df_re <- data |>
  filter(YEAR == 2002) |>
  reframe(
    n = sum(PERWT),
    .by = AGE
  ) |>
  arrange(AGE) |>
  left_join(
    age5categories |> select(age1, agecat),
    by = c("AGE" = "age1")
  )

# children by single age group

children <- df_re |>
  filter(AGE %in% 0:14)

# women by 5-yo age group

women <- df_re |>
  filter(agecat %in% 10:60) |>
  reframe(
    n = sum(n),
    .by = agecat
  )

clipr::write_clip(women)
clipr::write_clip(children)
