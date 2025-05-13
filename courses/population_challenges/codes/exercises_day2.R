#'---------------------------------------
#'@date 2025-05-13
#'@program EDSD
#'@course Population Challenges - Measuring social and generational inequalities
#'@professor Bernhard Binder-Hammer (VID)
#'@description Homework exercise - day 2
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

# libraries ---------------------------------------------------------------

library(pacman)
p_load(DemoKin, tidyverse, ggplot2, gganimate, patchwork, wpp2024, readxl)

# external functions

if(file.exists("UNWPP_data.R") == TRUE){
  source("UNWPP_data.R")
  source("UNWPP_download.R")
} else{
  source("codes/UNWPP_data.R")
  source("codes/UNWPP_download.R")
}


# Importing data from the UN ----------------------------------------------

# Downloading data from wpp website to have life table and age-specific fertility rates

download_wpp24(indicator = "mortality")
download_wpp24(indicator = "fertility")

# Importing an auxiliar dataset for choosing countries

data("countries")

countries <- countries |>
  filter(name %in% c("Spain","United Kingdom"))

# Life tables from wpp website

lt_female <- UNWPP_data(
  country = pull(countries[,2]),
  start_year = 1950,
  end_year = 2023,
  sex = "Female",
  indicator = "mortality"
)

lt_male <- UNWPP_data(
  country = pull(countries[,2]),
  start_year = 1950,
  end_year = 2023,
  sex = "Male",
  indicator = "mortality"
)

lt <- bind_rows(lt_female, lt_male) |>
  select(Location, Sex, year, AgeGrp, age, px)

# age-specific fertility rates from WPP website

fx <- UNWPP_data(
  country = pull(countries[,2]),
  start_year = 1950,
  end_year = 2023,
  sex = "Female",
  indicator = "fertility"
)

# population

data("popAge1dt") # importing data from WPP (wpp2024 package)

pop <- popAge1dt |>
  filter(
    name %in% pull(countries[,2]),
    year %in% 1950:2023
  ) |>
  select("Location" = name, year, age, "Female" = popF, "Male" = popM) |>
  pivot_longer(Female:Male, names_to = "Sex", values_to = "pop")

# putting everything together - it is worth to have the same matrix dimension for the kinship models

data <- lt |>
  left_join(
    fx |> mutate(Sex = "Female"),
    by = join_by(Location, Sex, year, age)
  ) |>
  left_join(
    pop,
    by = join_by(Location, Sex, year, age)
  ) |>
  mutate(across(everything(), ~ replace_na(.x,0)))

# Modeling kinship networks -----------------------------------------------

### Model: Two-sex time-variant kinship model

## Reshaping population

# Female - Spain
pop_sp_f <- data %>%
  filter(
    Sex == "Female",
    Location == "Spain"
  ) %>%
  select(age, year, pop) %>%
  pivot_wider(names_from = year, values_from = pop) %>%
  select(-age) %>%
  as.matrix()

# Female - UK
pop_uk_f <- data %>%
  filter(
    Sex == "Female",
    Location != "Spain"
  ) %>%
  select(age, year, pop) %>%
  pivot_wider(names_from = year, values_from = pop) %>%
  select(-age) %>%
  as.matrix()

# Male - Spain
pop_sp_m <- data %>%
  filter(
    Sex == "Male",
    Location == "Spain"
  ) %>%
  select(age, year, pop) %>%
  pivot_wider(names_from = year, values_from = pop) %>%
  select(-age) %>%
  as.matrix()

# Male - UK
pop_uk_m <- data %>%
  filter(
    Sex == "Male",
    Location != "Spain"
  ) %>%
  select(age, year, pop) %>%
  pivot_wider(names_from = year, values_from = pop) %>%
  select(-age) %>%
  as.matrix()

## Reshaping fertility

# Spain

asfr_sp <- data %>%
  filter(
    Location == "Spain",
    Sex == "Female"
  ) |>
  select(age, year, fx) %>%
  pivot_wider(names_from = year, values_from = fx) %>%
  select(-age) %>%
  as.matrix()

# UK

asfr_uk <- data %>%
  filter(
    Location != "Spain",
    Sex == "Female"
  ) |>
  select(age, year, fx) %>%
  pivot_wider(names_from = year, values_from = fx) %>%
  select(-age) %>%
  as.matrix()

## Reshaping mortality

# Female - Spain

px_sp_f <- data %>%
  filter(
    Sex == "Female",
    Location == "Spain"
  ) %>%
  select(age, year, px) %>%
  pivot_wider(names_from = year, values_from = px) %>%
  select(-age) %>%
  as.matrix()

# Female - UK

px_uk_f <- data %>%
  filter(
    Sex == "Female",
    Location != "Spain"
  ) %>%
  select(age, year, px) %>%
  pivot_wider(names_from = year, values_from = px) %>%
  select(-age) %>%
  as.matrix()

# Male - Spain

px_sp_m <- data %>%
  filter(
    Sex != "Female",
    Location == "Spain"
  ) %>%
  select(age, year, px) %>%
  pivot_wider(names_from = year, values_from = px) %>%
  select(-age) %>%
  as.matrix()

# Male - UK

px_uk_m <- data %>%
  filter(
    Sex != "Female",
    Location != "Spain"
  ) %>%
  select(age, year, px) %>%
  pivot_wider(names_from = year, values_from = px) %>%
  select(-age) %>%
  as.matrix()

## Running the model

# Spain

kin_sp <- kin2sex(
  pf = px_sp_f, # female mortality
  pm = px_sp_m, # male mortality
  ff = asfr_sp, # female fertility
  fm = asfr_sp, # male fertility - assumption that is the same than female one
  nf = pop_sp_f, # female age structure
  nm = pop_sp_m, # male age structure
  time_invariant = FALSE,
  output_period = 2015
)

# UK

kin_uk <- kin2sex(
  pf = px_uk_f, # female mortality
  pm = px_uk_m, # male mortality
  ff = asfr_uk, # female fertility
  fm = asfr_uk, # male fertility - assumption that is the same than female one
  nf = pop_uk_f, # female age structure
  nm = pop_uk_m, # male age structure
  time_invariant = FALSE,
  output_period = 2015
)

## Recategorizing kins

# vectors

vertical_asc <- c("ggm","gm","m","oa","ya","a")
vertical_desc <- c("d","gd","ggd","n","nos","nys")
horizontal <- c("coa","cya","c","os","ys","s")

kin_variables <- kin_sp$kin_summary |>
  mutate(country = "Spain") |>
  bind_rows(
    kin_uk$kin_summary |>
      mutate(country = "United Kingdom")
  ) |>
  mutate(
    vertical = case_when(kin %in% c(vertical_asc,vertical_desc) ~ 1, TRUE ~ 0),
    vertical_asc = case_when(kin %in% c(vertical_asc) ~ 1, TRUE ~ 0),
    vertical_desc = case_when(kin %in% c(vertical_desc) ~ 1, TRUE ~ 0),
    horizontal = case_when(kin %in% c(horizontal) ~ 1, TRUE ~ 0)
  ) |>
  # taking into account living relatives
  mutate(
    vertical = case_when(vertical == 1 ~ count_living, TRUE ~ 0),
    vertical_asc = case_when(vertical_asc == 1 ~ count_living, TRUE ~ 0),
    vertical_desc = case_when(vertical_desc == 1 ~ count_living, TRUE ~ 0),
    horizontal = case_when(horizontal == 1 ~ count_living, TRUE ~ 0)
  )

## creating summarized indicators

# summarizing indicators by age of focal and sex

kin_summary <- kin_variables |>
  reframe(
    sex_kin = "t",
    vertical = sum(vertical),
    vertical_asc = sum(vertical_asc),
    vertical_desc = sum(vertical_desc),
    horizontal = sum(horizontal),
    .by = c(country,age_focal)
  ) |>
  bind_rows(
    kin_variables |>
      reframe(
        vertical = sum(vertical),
        vertical_asc = sum(vertical_asc),
        vertical_desc = sum(vertical_desc),
        horizontal = sum(horizontal),
        .by = c(country,age_focal,sex_kin)
      )
  )

# creating dependency ratios

kin_summary <- kin_summary |>
  mutate(
    dr_vert_hori = vertical / horizontal,
    dr_vert_asc_hori = vertical_asc / horizontal,
    dr_vert_desc_hori = vertical_desc / horizontal,
  )


# Exploring this data -----------------------------------------------------

# Overall Dependency Ratios

overall_dr <- kin_summary |>
  reframe(
    year = 2015,
    vertical = sum(vertical),
    vertical_asc = sum(vertical_asc),
    vertical_desc = sum(vertical_desc),
    horizontal = sum(horizontal),
    .by = c(country,sex_kin)
  ) |>
  mutate(
    dr_vert_hori = vertical / horizontal,
    dr_vert_asc_hori = vertical_asc / horizontal,
    dr_vert_desc_hori = vertical_desc / horizontal,
  )

# graphing by sex and age - Total

kin_summary |>
  pivot_longer(
    dr_vert_hori:dr_vert_desc_hori,
    names_to = "type",
    values_to = "DR"
  ) |>
  left_join(
    overall_dr |>
      pivot_longer(
        dr_vert_hori:dr_vert_desc_hori,
        names_to = "type",
        values_to = "DR_overall"
      ) |>
      filter(sex_kin == "t") |>
      select(country, type, DR_overall),
    by = join_by(country,type)
  ) |>
  mutate(
    type = case_when(
      type == "dr_vert_hori" ~ "Vertical / Horizontal",
      type == "dr_vert_asc_hori" ~ "Vertical (Ascending) / Horizontal",
      type == "dr_vert_desc_hori" ~ "Vertical (Descending)/ Horizontal"
    ),
    sex_kin = case_when(sex_kin == "f" ~ "Female", sex_kin == "m" ~ "Male", TRUE ~ "Total")
  ) |>
  filter(type == "Vertical / Horizontal") |>
  ggplot() +
  aes(x = age_focal) +
  geom_line(
    aes(y = DR, color = sex_kin, group = interaction(sex_kin, sex_kin)),
    linewidth = 1.2
  ) +
  geom_point(
    aes(y = DR, color = sex_kin, group = interaction(sex_kin, sex_kin)),
    size = 3,
    alpha = .4
  ) +
  geom_line(
    aes(y = DR_overall),
    color = "grey44",
    linetype = "dashed",
    linewidth = 1.3
  ) +
  lemon::facet_rep_grid(.~country,repeat.tick.labels = TRUE, scales = "free_y") +
  geom_text(
    aes(x = 50, y = DR_overall*1.4, label = "KDR (Overall)", group = type),
    hjust = .5,
    check_overlap = TRUE,
    vjust = 0,
    color = "grey44"
  ) +
  labs(
    y = "KDR = Vertical relatives / Horizontal relatives",
    x = "Focal's age",
    title = "Kinship Dependency Ratios (KDR) over focal's life cicle\nby focal's sex - Overall",
    color = "",
    caption = "Source: UNDESA, World Population Prospects, Revision 2024."
  ) +
  scale_y_continuous(breaks = seq(0,round(max(kin_summary$dr_vert_hori)*1.1,1),1)) +
  # scale_x_continuous(breaks = seq(0,100),20) +
  scale_color_viridis_d(option = "D") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = .5, vjust = .5)
  )

ggsave(
  filename = file.path("courses","population_challenges","output","exercise2_kdr_overall.pdf"),
  device = "pdf",
  width = 10,
  height = 6.5
)

# graphing by sex and age - Descending

kin_summary |>
  pivot_longer(
    dr_vert_hori:dr_vert_desc_hori,
    names_to = "type",
    values_to = "DR"
  ) |>
  left_join(
    overall_dr |>
      pivot_longer(
        dr_vert_hori:dr_vert_desc_hori,
        names_to = "type",
        values_to = "DR_overall"
      ) |>
      filter(sex_kin == "t") |>
      select(country, type, DR_overall),
    by = join_by(country,type)
  ) |>
  mutate(
    type = case_when(
      type == "dr_vert_hori" ~ "Vertical / Horizontal",
      type == "dr_vert_asc_hori" ~ "Vertical (Ascending) / Horizontal",
      type == "dr_vert_desc_hori" ~ "Vertical (Descending)/ Horizontal"
    ),
    sex_kin = case_when(sex_kin == "f" ~ "Female", sex_kin == "m" ~ "Male", TRUE ~ "Total")
  ) |>
  filter(type == "Vertical (Descending)/ Horizontal") |>
  ggplot() +
  aes(x = age_focal) +
  geom_line(
    aes(y = DR, color = sex_kin, group = interaction(sex_kin, sex_kin)),
    linewidth = 1.2
  ) +
  geom_point(
    aes(y = DR, color = sex_kin, group = interaction(sex_kin, sex_kin)),
    size = 3,
    alpha = .4
  ) +
  geom_line(
    aes(y = DR_overall),
    color = "grey44",
    linetype = "dashed",
    linewidth = 1.3
  ) +
  lemon::facet_rep_grid(.~country,repeat.tick.labels = TRUE, scales = "free_y") +
  geom_text(
    aes(x = 50, y = DR_overall*1.4, label = "KDR (Overall)", group = type),
    hjust = .5,
    check_overlap = TRUE,
    vjust = 0,
    color = "grey44"
  ) +
  labs(
    y = "KDR = Vertical relatives / Horizontal relatives",
    x = "Focal's age",
    title = "Kinship Dependency Ratios (KDR) over focal's life cicle\nby focal's sex - Descending",
    color = "",
    caption = "Source: UNDESA, World Population Prospects, Revision 2024."
  ) +
  scale_y_continuous(breaks = seq(0,round(max(kin_summary$dr_vert_hori)*1.1,1),1)) +
  # scale_x_continuous(breaks = seq(0,100),20) +
  scale_color_viridis_d(option = "D") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = .5, vjust = .5)
  )

ggsave(
  filename = file.path("courses","population_challenges","output","exercise2_kdr_descending.pdf"),
  device = "pdf",
  width = 10,
  height = 6.5
)

# graphing by sex and age - Ascending

kin_summary |>
  pivot_longer(
    dr_vert_hori:dr_vert_desc_hori,
    names_to = "type",
    values_to = "DR"
  ) |>
  left_join(
    overall_dr |>
      pivot_longer(
        dr_vert_hori:dr_vert_desc_hori,
        names_to = "type",
        values_to = "DR_overall"
      ) |>
      filter(sex_kin == "t") |>
      select(country, type, DR_overall),
    by = join_by(country,type)
  ) |>
  mutate(
    type = case_when(
      type == "dr_vert_hori" ~ "Vertical / Horizontal",
      type == "dr_vert_asc_hori" ~ "Vertical (Ascending) / Horizontal",
      type == "dr_vert_desc_hori" ~ "Vertical (Descending)/ Horizontal"
    ),
    sex_kin = case_when(sex_kin == "f" ~ "Female", sex_kin == "m" ~ "Male", TRUE ~ "Total")
  ) |>
  filter(type == "Vertical (Ascending) / Horizontal") |>
  ggplot() +
  aes(x = age_focal) +
  geom_line(
    aes(y = DR, color = sex_kin, group = interaction(sex_kin, sex_kin)),
    linewidth = 1.2
  ) +
  geom_point(
    aes(y = DR, color = sex_kin, group = interaction(sex_kin, sex_kin)),
    size = 3,
    alpha = .4
  ) +
  geom_line(
    aes(y = DR_overall),
    color = "grey44",
    linetype = "dashed",
    linewidth = 1.3
  ) +
  lemon::facet_rep_grid(.~country,repeat.tick.labels = TRUE, scales = "free_y") +
  geom_text(
    aes(x = 50, y = DR_overall*1.4, label = "KDR (Overall)", group = type),
    hjust = .5,
    check_overlap = TRUE,
    vjust = 0,
    color = "grey44"
  ) +
  labs(
    y = "KDR = Vertical relatives / Horizontal relatives",
    x = "Focal's age",
    title = "Kinship Dependency Ratios (KDR) over focal's life cicle\nby focal's sex - Ascending",
    color = "",
    caption = "Source: UNDESA, World Population Prospects, Revision 2024."
  ) +
  scale_y_continuous(breaks = seq(0,round(max(kin_summary$dr_vert_hori)*1.1,1),1)) +
  # scale_x_continuous(breaks = seq(0,100),20) +
  scale_color_viridis_d(option = "D") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = .5, vjust = .5)
  )

ggsave(
  filename = file.path("courses","population_challenges","output","exercise2_kdr_ascending.pdf"),
  device = "pdf",
  width = 10,
  height = 6.5
)
