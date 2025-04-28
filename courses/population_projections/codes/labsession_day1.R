#'---------------------------------------
#'@date 2025-04-28
#'@program EDSD
#'@course Population Projections
#'@professor Marilia Nepomuceno (MPIDR)
#'@description Course's exercises - day 1
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

if(!getwd() == "C://Users//DELL//OneDrive//Estudos//Mestrado - EDSD//Course//EDSD_things"){
  setwd("C://Users//DELL//OneDrive//Estudos//Mestrado - EDSD//Course//EDSD_things")
}

# Libraries ---------------------------------------------------------------

library(pacman)
p_load(haven, tidyverse, data.table,readr, patchwork)

# external functions
source("codes/UNWPP_download.R")

# Downloading data from WPP -----------------------------------------------

download_wpp_previous(wpp_version_year = 2010, indicator = "pop_agesex_1x1_mv")
download_wpp_previous(wpp_version_year = 2019, indicator = "pop_agesex_1x1_mv")

# Importing data ----------------------------------------------------------

df_wpp_2010 <- read_csv(
  file.path("data","WPP2010_PopulationByAgeSex_Medium.csv"),
  show_col_types = FALSE
) |>
  mutate(WPP_revision = 2010)


df_wpp_2019 <- read_csv(
  file.path("data","WPP2019_PopulationByAgeSex_Medium.csv"),
  show_col_types = FALSE
) |>
  mutate(WPP_revision = 2019)

df_wpp <- bind_rows(df_wpp_2010, df_wpp_2019)
rm(df_wpp_2010, df_wpp_2019)

# Data handling -------------------------------------------------------------
colnames(df_wpp)

# filter data

df_wpp <- df_wpp |>
  select(everything(), "Male" = PopMale, "Female" = PopFemale, "Total" = PopTotal) |>
  filter(
  Location == "India",
  Time %in% c(2010,2019,2070)
) |>
  pivot_longer(Male:Total, names_to = "sex", values_to = "N")

# All the data over age 80 will be consider 80+

df_wpp <- df_wpp |>
  select(-AgeGrp, - AgeGrpSpan) |>
  mutate(AgeGrpStart = case_when(AgeGrpStart >= 80 ~ 80, TRUE ~ AgeGrpStart)) |>
  mutate(
    N = sum(N),
    .by = c(Location, Variant, Time, WPP_revision, sex, AgeGrpStart)
  ) |>
  distinct()

# relative age distribution

df_wpp <- df_wpp |>
  mutate(
    prop = 100*N/sum(N),
    .by = c(Location, Variant, Time, WPP_revision, sex)
  )


# Graphs ------------------------------------------------------------------

# 1 - Age distribution by revision - 2070

(df_wpp |>
  filter(Time == 2070, sex == "Total") |>
  ggplot() +
  aes(x = AgeGrpStart, y = N, color = as.factor(WPP_revision)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 5, alpha = .5) +
  coord_cartesian(ylim = c(0,125000)) +
  scale_y_continuous(breaks = seq(0,125000,25000)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  labs(
    title = "Absolute Distribution - India (2070)",
    color = "",
    y = "Population (Absolute)",
    x = "Age (5 years-age group)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  theme(
    plot.title = element_text(face = "bold", colour = "grey44"),
    axis.title = element_text(face = "bold", colour = "grey44"),
    strip.text = element_text(face = "bold", colour = "grey44"),
    axis.text = element_text(colour = "grey44"),
    legend.position = "bottom"
  )
) +
  (
    df_wpp |>
      filter(Time == 2070, sex == "Total") |>
      ggplot() +
      aes(x = AgeGrpStart, y = prop, color = as.factor(WPP_revision)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 5, alpha = .5) +
      coord_cartesian(ylim = c(0,8)) +
      scale_y_continuous(breaks = seq(0,12,1)) +
      scale_x_continuous(breaks = seq(0,100,10)) +
      labs(
        title = "Relative Distribution - India (2070)",
        color = "",
        y = "Population (Relative, %)",
        x = "Age (5 years-age group)"
      ) +
      theme_minimal(base_size = 14) +
      scale_color_brewer(type = "qual", palette = "Set2") +
      theme(
        plot.title = element_text(face = "bold", colour = "grey44"),
        axis.title = element_text(face = "bold", colour = "grey44"),
        strip.text = element_text(face = "bold", colour = "grey44"),
        axis.text = element_text(colour = "grey44"),
        legend.position = "bottom"
      )
  )

ggsave(
  filename = "courses/population_projections/output/labsession1_india_2070_totals.pdf",
  device = "pdf",
  width = 10,
  height = 6.5
)

# 2 - Ratio between 2019/2010
# Assumption - if the scenarios are the same, we would expect the same pop. size and distribution

(df_wpp |>
    filter(Time == 2070, sex == "Total") |>
    select(-prop) |>
    pivot_wider(names_from = WPP_revision, values_from = N, names_prefix = "Revision_") |>
    mutate(ratio = Revision_2019/Revision_2010) |>
    ggplot() +
    aes(x = AgeGrpStart, y = ratio) +
    geom_line(linewidth = 1.2, color = "grey44") +
    geom_point(size = 5, alpha = .5, color = "grey44") +
    coord_cartesian(ylim = c(0.8,1.1)) +
    scale_x_continuous(breaks = seq(0,100,10)) +
    geom_hline(yintercept = 1, color = "orange2", linetype = "dashed", linewidth = 1.3) +
    labs(
      title = "Absolute Distribution - India (2070)",
      color = "",
      y = "N (Revision of 2019) / N (Revision of 2010)",
      x = "Age (5 years-age group)"
    ) +
    theme_minimal(base_size = 14) +
    scale_color_brewer(type = "qual", palette = "Set2") +
    theme(
      plot.title = element_text(face = "bold", colour = "grey44"),
      axis.title = element_text(face = "bold", colour = "grey44"),
      strip.text = element_text(face = "bold", colour = "grey44"),
      axis.text = element_text(colour = "grey44"),
      legend.position = "bottom"
    )
) +
  (
    df_wpp |>
      filter(Time == 2070, sex == "Total") |>
      select(-N) |>
      pivot_wider(names_from = WPP_revision, values_from = prop, names_prefix = "Revision_") |>
      mutate(ratio = Revision_2019/Revision_2010) |>
      ggplot() +
      aes(x = AgeGrpStart, y = ratio) +
      geom_line(linewidth = 1.2, color = "grey44") +
      geom_point(size = 5, alpha = .5, color = "grey44") +
      coord_cartesian(ylim = c(0.8,1.1)) +
      scale_x_continuous(breaks = seq(0,100,10)) +
      geom_hline(yintercept = 1, color = "orange2", linetype = "dashed",linewidth = 1.3) +
      labs(
        title = "Absolute Distribution - India (2070)",
        color = "",
        y = "N (Revision of 2019) / N (Revision of 2010)",
        x = "Age (5 years-age group)"
      ) +
      theme_minimal(base_size = 14) +
      scale_color_brewer(type = "qual", palette = "Set2") +
      theme(
        plot.title = element_text(face = "bold", colour = "grey44"),
        axis.title = element_text(face = "bold", colour = "grey44"),
        strip.text = element_text(face = "bold", colour = "grey44"),
        axis.text = element_text(colour = "grey44"),
        legend.position = "bottom"
      )
  )

ggsave(
  filename = "courses/population_projections/output/labsession1_india_2070_ratio.pdf",
  device = "pdf",
  width = 10,
  height = 6.5
)

# 3 - Age distribution by revision - 2010, 2019, 2070

(df_wpp |>
    filter(sex == "Total") |>
    ggplot() +
    aes(x = AgeGrpStart, y = N, color = as.factor(WPP_revision)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2, alpha = .5) +
    coord_cartesian(ylim = c(0,125000)) +
    scale_y_continuous(breaks = seq(0,125000,25000)) +
    scale_x_continuous(breaks = seq(0,100,10)) +
    lemon::facet_rep_wrap(.~Time, repeat.tick.labels = TRUE) +
    labs(
      title = "Absolute Distribution - India (2070)",
      color = "",
      y = "Population (Absolute)",
      x = "Age (5 years-age group)"
    ) +
    theme_minimal(base_size = 12) +
    scale_color_brewer(type = "qual", palette = "Set2") +
    theme(
      plot.title = element_text(face = "bold", colour = "grey44"),
      axis.title = element_text(face = "bold", colour = "grey44"),
      strip.text = element_text(face = "bold", colour = "grey44"),
      axis.text = element_text(colour = "grey44"),
      legend.position = "bottom"
    )
) /
  (
    df_wpp |>
      filter(sex == "Total") |>
      ggplot() +
      aes(x = AgeGrpStart, y = prop, color = as.factor(WPP_revision)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2, alpha = .5) +
      # coord_cartesian(ylim = c(0,8)) +
      scale_y_continuous(breaks = seq(0,20,1)) +
      scale_x_continuous(breaks = seq(0,100,10)) +
      lemon::facet_rep_wrap(.~Time, repeat.tick.labels = TRUE) +
      labs(
        title = "Relative Distribution - India (2070)",
        color = "",
        y = "Population (Relative, %)",
        x = "Age (5 years-age group)"
      ) +
      theme_minimal(base_size = 12) +
      scale_color_brewer(type = "qual", palette = "Set2") +
      theme(
        plot.title = element_text(face = "bold", colour = "grey44"),
        axis.title = element_text(face = "bold", colour = "grey44"),
        strip.text = element_text(face = "bold", colour = "grey44"),
        axis.text = element_text(colour = "grey44"),
        legend.position = "bottom"
      )
  )

ggsave(
  filename = "courses/population_projections/output/labsession1_india_2010to2070_totals.pdf",
  device = "pdf",
  width = 10,
  height = 6.5
)

# 4 - Ratio between 2019/2010
# Assumption - if the scenarios are the same, we would expect the same pop. size and distribution in EACH YEAR

(df_wpp |>
    filter(sex == "Total") |>
    select(-prop) |>
    pivot_wider(names_from = WPP_revision, values_from = N, names_prefix = "Revision_") |>
    mutate(ratio = Revision_2019/Revision_2010) |>
    ggplot() +
    aes(x = AgeGrpStart, y = ratio, color = as.factor(Time)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 5, alpha = .5) +
    coord_cartesian(ylim = c(0.8,1.1)) +
    scale_x_continuous(breaks = seq(0,100,10)) +
    geom_hline(yintercept = 1,  color = "grey44", linetype = "dashed", linewidth = 1.3) +
    labs(
      title = "Absolute Distribution - India (2070)",
      color = "",
      y = "N (Revision of 2019) / N (Revision of 2010)",
      x = "Age (5 years-age group)"
    ) +
    theme_minimal(base_size = 14) +
    scale_color_brewer(type = "qual", palette = "Set2") +
    theme(
      plot.title = element_text(face = "bold", colour = "grey44"),
      axis.title = element_text(face = "bold", colour = "grey44"),
      strip.text = element_text(face = "bold", colour = "grey44"),
      axis.text = element_text(colour = "grey44"),
      legend.position = "bottom"
    )
) +
  (
    df_wpp |>
      filter(sex == "Total") |>
      select(-N) |>
      pivot_wider(names_from = WPP_revision, values_from = prop, names_prefix = "Revision_") |>
      mutate(ratio = Revision_2019/Revision_2010) |>
      ggplot() +
      aes(x = AgeGrpStart, y = ratio, color = as.factor(Time)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 5, alpha = .5) +
      coord_cartesian(ylim = c(0.8,1.1)) +
      scale_x_continuous(breaks = seq(0,100,10)) +
      geom_hline(yintercept = 1, color = "grey44", linetype = "dashed",linewidth = 1.3) +
      labs(
        title = "Absolute Distribution - India (2070)",
        color = "",
        y = "N (Revision of 2019) / N (Revision of 2010)",
        x = "Age (5 years-age group)"
      ) +
      theme_minimal(base_size = 14) +
      scale_color_brewer(type = "qual", palette = "Set2") +
      theme(
        plot.title = element_text(face = "bold", colour = "grey44"),
        axis.title = element_text(face = "bold", colour = "grey44"),
        strip.text = element_text(face = "bold", colour = "grey44"),
        axis.text = element_text(colour = "grey44"),
        legend.position = "bottom"
      )
  )

ggsave(
  filename = "courses/population_projections/output/labsession1_india_2010to2070_ratio.pdf",
  device = "pdf",
  width = 10,
  height = 6.5
)
