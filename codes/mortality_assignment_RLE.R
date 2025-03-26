#'---------------------------------------
#'@date 2025-03-25
#'@program EDSD
#'@course Mortality II
#'@professor Cosmo Strozza
#'@description Assignment
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

# Libraries ---------------------------------------------------------------

library(pacman)
p_load(haven, tidyverse, wpp2022, wpp2024)

# external functions
source("codes/UNWPP_data.R")
source("codes/UNWPP_download.R")


# Downloading data from WPP -----------------------------------------------

download_wpp24(indicator = "mortality")


# Importing data ----------------------------------------------------------

lt_female <- UNWPP_data(
  country = c("Azerbaijan","Georgia","Armenia"),
  start_year = 2000,
  end_year = 2023,
  sex = "Female",
  indicator = "mortality"
)

lt_male <- UNWPP_data(
  country = c("Azerbaijan","Georgia","Armenia"),
  start_year = 2000,
  end_year = 2023,
  sex = "Male",
  indicator = "mortality"
)

lt <- bind_rows(lt_female, lt_male)
rm(lt_female, lt_male)
# RLE ---------------------------------------------------------------------

# Dataframe for each country and the age of retirement
age_retirement <- tibble(
  year = rep(seq(2000,2023,1),3),
  country = c(rep("Georgia",2024-2000),rep("Armenia",2024-2000),rep("Azerbaijan",2024-2000)),
  Male = c(rep(65,2024-2000),rep(65,2024-2000),rep(63,2024-2000)),
  Female = c(rep(60,2024-2000),rep(63,2024-2000),rep(60,2024-2000))
) |>
  pivot_longer(Male:Female, names_to = "sex", values_to = "retirement_age") |>
  mutate(age_law = case_when(
    country == "Azerbaijan" & year >= 2017 ~ retirement_age + (year - 2017 + 1) * 0.5,
    TRUE ~ retirement_age
  )) |>
  # adjusting age at retirement and rounding it
  mutate(
    age_law = case_when(
      age_law > 65 ~ 65, TRUE ~ round(age_law-0.01,0)
    )
  )

# join data frame with age of retirement to lt

lt <- lt |>
  left_join(
    age_retirement,
    by = c("year", "Sex" = "sex", "Location" = "country")
  )

# Calculating RLE

rle <- lt |>
  group_by(year, country = Location, sex = Sex) |>
  reframe(
    rle = case_when(age == retirement_age ~ ex, TRUE ~ 0),
    rle_new_law = case_when(age == age_law ~ ex, TRUE ~ 0),
    rle_label = case_when(age == age_law & country == "Azerbaijan" ~ "Indexation", TRUE ~ "No indexation"),
  ) |>
  distinct() |>
  group_by(year, country, sex, rle_label) |>
  reframe(
    rle = sum(rle),
    rle_new_law = sum(rle_new_law)) |>
  mutate(rle = case_when(rle == 0 ~ rle_new_law, TRUE ~ rle)) |>
  mutate(rle_new_law = sum(rle_new_law), .by = c(year, country, sex)) |>
  mutate(rle = case_when(rle == 0 ~ rle_new_law, TRUE ~ rle)) |>
  select(-rle_new_law)

# Plotting it -------------------------------------------------------------

# creating texts for graphic

ages_labs <- tibble(
  sex = c("Male","Female"),
  labs = c(
    "Retirement age:\nArmenia - 65\nAzerbaijan - 63\nGeorgia - 65",
    "Retirement age:\nArmenia - 63\nAzerbaijan - 60\nGeorgia - 60"
  ),
  labs_index = c(
    "Retirement age (no indexation):\n-Armenia - 65\n-Azerbaijan - 63\n-Georgia - 65",
    "Retirement age (no indexation):\n-Armenia - 63\n-Azerbaijan - 60\n-Georgia - 60"
  )
)

# Without take into account increase of age at retirement in Azerbaijan
rle |>
  left_join(
    ages_labs,
    by = join_by(sex)
  ) |>
  filter(rle_label == "No indexation") |>
  ggplot() +
  aes(x = year, y = rle, color = country) +
  geom_point(size = 2, alpha = .5) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  geom_hline(yintercept = 14.5, linetype = "dotted", color = "grey44", linewidth = 1.3) +
  lemon::facet_rep_wrap(.~sex, repeat.tick.labels = TRUE) +
  coord_cartesian(ylim = c(10,24)) +
  scale_y_continuous(breaks = seq(8,25,2)) +
  labs(
    color = "",
    y = "Remaining Life Expectancy (RLE)",
    x = "Year"
  ) +
  geom_text(
    aes(
      x = 2000,
      y = 15,
      label = "e(t,Denmark) = 14.5"
    ),
    color = "grey44",
    hjust = 0
  ) +
  geom_text(aes(x = 2000, y = Inf, label = labs, group = sex),
            hjust = 0,
            vjust = 1.4,
            color = "grey44") +
  theme_minimal(base_size = 20) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  theme(
    axis.title = element_text(face = "bold", colour = "grey44"),
    strip.text = element_text(face = "bold", colour = "grey44"),
    axis.text = element_text(colour = "grey44"),
    legend.position = "bottom"
  )

ggsave(
  filename = "outputs/mortality2_rle.pdf",
  device = "pdf",
  width = 10,
  height = 6.5
)

# With take into account increase of age at retirement in Azerbaijan
rle |>
  left_join(
    ages_labs,
    by = join_by(sex)
  ) |>
  ggplot() +
  aes(x = year, y = rle, color = country) +
  geom_point(size = 2, alpha = .5, show.legend = FALSE) +
  geom_hline(yintercept = 14.5, linetype = "dotted", color = "grey44", linewidth = 1.3) +
  geom_smooth(aes(linetype = fct_rev(rle_label)), se = FALSE, linewidth = 1.2) +
  lemon::facet_rep_wrap(.~sex, repeat.tick.labels = TRUE) +
  geom_text(
    aes(
      x = 2000,
      y = 15,
      label = "e(t,Denmark) = 14.5"
    ),
    color = "grey44",
    hjust = 0
  ) +
  geom_text(aes(x = 2000, y = Inf, label = labs_index, group = sex),
            hjust = 0,
            vjust = 1.4,
            color = "grey44") +
  coord_cartesian(ylim = c(10,24)) +
  scale_y_continuous(breaks = seq(8,25,2)) +
  labs(
    color = "",
    linetype = "",
    y = "Remaining Life Expectancy (RLE)",
    x = "Year"
  ) +
  theme_minimal(base_size = 20) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  theme(
    axis.title = element_text(face = "bold", colour = "grey44"),
    strip.text = element_text(face = "bold", colour = "grey44"),
    axis.text = element_text(colour = "grey44"),
    legend.position = "bottom"
  )

ggsave(
  filename = "outputs/mortality2_rle_scenarios.pdf",
  device = "pdf",
  width = 10,
  height = 6.5
)
