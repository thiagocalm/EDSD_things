# Setup -------------------------------------------------------------------
library(here)
wd <- here()
setwd(wd)

load(file.path("courses","decomposition","day3","data","Arriaga_COVID_USA.RData"))

# packages
library(tidyverse)
library(data.table)

# Compare life expectancies  ----------------------------------------------

e0_2020 <- data %>%
  filter(year==2020,age==0,sex==0) %>%
  pull(ex) %>%
  unique()

e0_2019 <- data %>%
  filter(year==2019,age==0,sex==0) %>%
  pull(ex) %>%
  unique()

e0_2020 - e0_2019

# Decomposition -----------------------------------------------------------

# Age

data_nocause <- data %>%
  select(!c(cause,prop)) %>%
  filter(sex == 0) |>
  distinct()

# 2020

lx_2020 <- data_nocause |>
  filter(year == 2020) |>
  select(lx) |>
  pull()

Lx_2020 <- data_nocause |>
  filter(year == 2020) |>
  select(Lx) |>
  pull()

Tx_2020 <- data_nocause |>
  filter(year == 2020) |>
  select(Tx) |>
  pull()

# 2019

lx_2019 <- data_nocause |>
  filter(year == 2019) |>
  select(lx) |>
  pull()

Lx_2019 <- data_nocause |>
  filter(year == 2019) |>
  select(Lx) |>
  pull()

Tx_2019 <- data_nocause |>
  filter(year == 2019) |>
  select(Tx) |>
  pull()

### applying it

# direct effect

direct <- (lx_2019/lx_2019[1]) * ((Lx_2020/lx_2020) - (Lx_2019/lx_2019))

# indirect effect
indirect <- (Tx_2020[-1]/lx_2019[1]) * ((lx_2019[-length(lx_2019)]/lx_2020[-length(lx_2020)]) - (lx_2019[-1]/lx_2020[-1]))
indirect <- c(indirect,0)

# total effect

diff_dec_ex <- direct + indirect

sum(dff_dec_ex)

# Extension of Arriaga for cause-specific mortality -----------------------


data_cause <- data %>%
  # select(!c(cause,prop)) %>%
  filter(sex == 0) |>
  distinct()

# vector with causes

causes <- data_cause |> pull(cause) |> unique() |> as.character()
i = 1
for(i in seq_along(causes)){

  df <- data_cause |>
    filter(cause %in% causes[i])

  # 2020

  prop_2020 <- df |>
    filter(year == 2020) |>
    pull(prop) |>
    as.numeric()

  mx_2020 <- df |>
    filter(year == 2020) |>
    pull(mx)

  # 2019
  prop_2019 <- df |>
    filter(year == 2019) |>
    pull(prop) |>
    as.numeric()

  mx_2019 <- df |>
    filter(year == 2019) |>
    pull(mx)

  # contribution
  diff_cause_ex <- diff_dec_ex *  ( (prop_2020*mx_2020)-(prop_2019*mx_2019)/ (mx_2020-mx_2019))

  # putting everything together...
  if(i == 1){
    diff_by_cause <- data.frame(diff_cause_ex)
  } else{
    diff_by_cause <- bind_cols(diff_by_cause,diff_cause_ex)
  }
  colnames(diff_by_cause)[i] <- causes[i]

  # next loop...
  rm(diff_cause_ex)
}

# creating last dataset
diff <- tibble(
  "age" = data_cause |> select(age) |> distinct() |> pull(),
  "all_causes" = diff_dec_ex
) |>
  bind_cols(diff_by_cause)

