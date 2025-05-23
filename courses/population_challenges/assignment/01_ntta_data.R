#'---------------------------------------
#'@date 2025-05-23
#'@program EDSD
#'@course Population Challenges - Assignment
#'@professor Tommy, Bernhard and Carole
#'@description Importing and handling time data
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

# libraries ---------------------------------------------------------------

library(pacman)
p_load(DemoKin, tidyverse, ggplot2, gganimate, patchwork, wpp2024, readxl)

# # external functions
#
# if(file.exists("UNWPP_data.R") == TRUE){
#   source("UNWPP_data.R")
#   source("UNWPP_download.R")
# } else{
#   source("codes/UNWPP_data.R")
#   source("codes/UNWPP_download.R")
# }


# Importing data from the UN ----------------------------------------------

ntta <- read_csv(file.path("data","MTUS NTTA_long_Time.csv"),col_names = TRUE)


# Data handling -----------------------------------------------------------

# filtering selected countries

cnt <- c("United Kingdom","Spain")

ntta <- ntta |>
  filter(country %in% cnt)

# creating Net Contribution

ntta <- ntta |>
  mutate(
    across(everything(), ~ replace_na(.x,0))
  ) |>
  mutate(
    NT = (CT - PT) / 60
  )

# selecting last data available for each country

ntta <- ntta |>
  arrange(country,sex, age, desc(year)) |>
  mutate(
    rank = row_number(),
    .by = c(country,sex, age)
  ) |>
  filter(rank == 1) |>
  select(-rank)

# Plotting it -------------------------------------------------------------

ntta |>
  filter(!sex == "total") |>
  ggplot() +
  aes(x = age, y = NT, color = country, linetype = sex) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, linewidth = 1.3, linetype = "dashed") +
  labs(
    y = "Net transfer = Total Consumption - Total Production",
    x = "Age",
    color = "",
    linewidth = "",
    caption = "Source: NTTA."
  ) +
  scale_x_continuous(breaks = seq(0,80,5)) +
  scale_y_continuous(breaks = seq(-5,10,1)) +
  scale_color_brewer(palette = "Set2") +
  theme_light(base_size = 14) +
  theme(
    panel.border = element_rect(color = "grey95"),
    plot.title = element_text(face = "bold", hjust = .5, vjust = .5),
    legend.title = element_blank(),
    legend.position = "top"
  )

