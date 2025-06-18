options(scipen = 999999)
rm(list = ls())
invisible(gc())
# Packages ----------------------------------------------------------------

library(pacman)
p_load(demogsurv,tidyverse,DHS.rates,haven)
# Bruno's package
source("codes/tabexp.R")

# Importing data ----------------------------------------------------------
country_code <- "GU"
# irfile <- read_dta(file.path("data","DHS_scrambled.zip",paste0(country_code,"_sbd_group.zip"),"GUIR71FL."))
irfile <- read_dta(file.path("data","GU_sbd_group","GUIR71FL_sbd.dta"))

# Estimating fertility ----------------------------------------------------

# Using demogsurv
asfr <- calc_asfr(irfile, tips=c(0, 3)) # ASFRs for the last 3 years
tfr <- calc_tfr(irfile, tips=c(0,3)) # TFR for the last 3 years
asfr
tfr

# Using DHS.rates

asfr_2 <- fert(irfile,Indicator="asfr")
tfr_2 <- fert(irfile,Indicator="tfr")

# Using tabexp

asfr_3 <- tabexp(irfile) |>
  filter(
    YBS <= 2,
    age_group != "[10,15)"
  ) |>
  reframe(
    events = sum(events),
    exposure_py = sum(exposure_py),
    .by = c(age_group)
  ) |>
  mutate(asfr = events/exposure_py)

tfr_3 <- sum(asfr_2$asfr * 5)

# Fertility trends - comparing surveys -----------------------------------

files <- list.files(file.path("data","GU_sbd_group"))
i = 1

for(i in seq_along(files)){
  # import data
  irfile <- read_dta(file.path("data","GU_sbd_group",files[i]))
  list_var <- colnames(irfile)
  if(!"v021" %in% list_var){
    irfile <- irfile |>
      mutate(v021 = v001)
  }
  # TFR trends 3-years
  tfr_3 <- calc_tfr(irfile, tips = c(0,3),strata = NULL)
  # TFR trends 10-years
  tfr_10 <- calc_tfr(irfile, tips = 0:10,strata = NULL)
  # computing year of survey
  ly<-mean(irfile$v008/12+1900)
  tfr_3$tips <- 1.5
  tfr_10$tips <- as.numeric(levels(tfr_10$tips))
  tfr_10$year <- round(ly-tfr_10$tips-0.5,0)
  tfr_3$year <- round(ly-tfr_3$tips,0)

  # add survey variable
  tfr_3 <- tfr_3 |>
    mutate(survey = str_sub(files[i], end=-9))
  tfr_10 <- tfr_10 |>
    mutate(survey = str_sub(files[i], end=-9))

  # pooling everything up
  if(i == 1){
    tfr3 <- tfr_3
    tfr10 <- tfr_10
  } else{
    tfr3 <- tfr3 |>
      bind_rows(tfr_3)

    tfr10 <- tfr10 |>
      bind_rows(tfr_10)
  }
  # next loop
  rm(tfr_3,tfr_10)
  print(paste0("finished the loop number: ",i,"!!!"))
}

# plotting it...

plot1 <- tfr10 |>
  ggplot() +
  aes(x = year, y = tfr, color = survey) +
  geom_point(size = 3) +
  geom_line(linewidth = 1.1) +
  geom_line(data = tfr3, color = "black",linewidth = 1.3) +
  geom_point(data = tfr3, color = "black", size = 4) +
  geom_smooth(mapping=aes(group=1), se=FALSE, colour='red',,linewidth = 1.3) +
  scale_x_continuous(breaks = seq(min(tfr10$year)-5,max(tfr10$year)+2,5)) +
  labs(
    title = "Trends in TFR - Guatemala",
    caption = "Source: DHS.",
    x = "Calendar year",
    y = "TFR"
  ) +
  theme_bw(base_size = 12)
plot1
