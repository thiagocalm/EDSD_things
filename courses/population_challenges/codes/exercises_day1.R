#'---------------------------------------
#'@date 2025-05-12
#'@program EDSD
#'@course Population Challenges - Measuring social and generational inequalities
#'@professor Bernhard Binder-Hammer (VID)
#'@description Homework exercise - day 1
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

# libraries ---------------------------------------------------------------

library(pacman)
p_load(tidyverse, wpp2024, readxl)

# National Accounts data --------------------------------------------------
# Source: Eurostat

# Importing data

na_df <- read_xlsx(
  file.path("courses","population_challenges","input","eurostat_national_account.xlsx"),
  sheet = "Sheet 5",
  range = "A11:AH48"
)

## Data handling

# Selecting columns and rows

col <- c("TIME",as.character(seq(2008,2023,1)))

na_df <- na_df[2:nrow(na_df),] |>
  select(any_of(col))

# renaming column and working as a long dataframe

na_df <- na_df |>
  rename("country" = TIME) |>
  pivot_longer(
    cols = col[2]:col[length(col)],
    values_to = "income_gross",
    names_to = "year"
  ) |>
  mutate(across(year:income_gross, ~ as.numeric(.)))

# Harmonized Index of Consumer Prices (HICP) --------------------------------
# Source: Eurostat
# Harmonized means that it is a harmonized index for all of the countries...
# however, for comparison reasons, we still needing to have a rate between them

## Importing data

hicp <- read_xlsx(
  file.path("courses","population_challenges","input","eurostat_harmonized_icp.xlsx"),
  sheet = "Sheet 1",
  range = "A9:AH55"
)

## Data handling

# Selecting columns and rows

col <- c("TIME",as.character(seq(2008,2023,1)))

hicp <- hicp[2:nrow(hicp),] |>
  select(any_of(col))

# renaming column and working as a long dataframe

hicp <- hicp |>
  rename("country" = TIME) |>
  mutate(across(col[2]:col[length(col)], ~ as.numeric(.))) |>
  pivot_longer(
    cols = col[2]:col[length(col)],
    values_to = "hicp",
    names_to = "year"
  ) |>
  mutate(year = as.numeric(year))


# Population data ---------------------------------------------------------
# Source: UN/WPP data
# Total population by year

# Importing data

data("pop1")

## Data handling

# Selecting cols and rows

col <- c("name",as.character(seq(2008,2023,1)))

pop1 <- pop1 |>
  select(any_of(col)) |>
  pivot_longer(
    cols = col[2]:col[length(col)],
    names_to = "year",
    values_to = "pop"
  ) |>
  rename("country" = name) |>
  mutate(year = as.numeric(year))


# Binding all the data ----------------------------------------------------

# countries selected

countries <- c("Greece", "Estonia")

# binding data
df <- na_df |>
  filter(country %in% countries) |>
  # joining data from HICP
  left_join(
    hicp |>
      filter(country %in% countries),
    by = join_by(country, year)
  ) |>
  # joining data from pop
  left_join(
    pop1 |>
      filter(country %in% countries),
    by = join_by(country, year)
  )

# handling data

df <- df |>
  mutate(
    income_gross = income_gross * 1000000,
    pop = pop * 1000,
    hicp = hicp / 100
  )

# Estimating Gross disposable income per capita

df <- df |>
  mutate(
    dipc = (income_gross / hicp) / pop
  )


# Graphic -----------------------------------------------------------------

df |>
  ggplot() +
  aes(x = year, y = dipc, color = country, group = interaction(country, country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 5, alpha = .4) +
  labs(
    y = "Euro per capita, real (HICP, 2015 = 100)",
    title = "Adjusted net disposable income per capita",
    color = "",
    caption = "Source: Eurostat, ESA 2010, annual sector accounts, household sector. UNDESA, World Population Prospects, Revision 2024."
  ) +
  scale_x_continuous(breaks = seq(2008,2023),1) +
  scale_y_continuous(breaks = seq(round(min(df$dipc)*.95,0),round(max(df$dipc)*1.05,0),1000)) +
  scale_color_viridis_d(option = "H") +
  theme_minimal(base_size = 18) +
  theme(
    axis.title.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = .5, vjust = .5)
  )
