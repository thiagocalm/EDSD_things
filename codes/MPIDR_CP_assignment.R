#'---------------------------------------
#'@date 2024-10-18
#'@program EDSD
#'@course Computer Programming E140
#'@professor Christian Dudel
#'@description Assignment 1
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

# Libraries ---------------------------------------------------------------

library(pacman)
p_load(haven, tidyverse, HMDHFDplus)

# Exercise 1 --------------------------------------------------------------

# Data URL
url <- "https://www.diw.de/documents/dokumentenarchiv/17/diw_01.c.412698.de/soep_lebensz_en.zip"

# Importing data
temp <- tempfile() # creating a temporary file
download.file(url,temp) # downloading data and storing in the temporary file
soep <- read_dta(unz(temp, "soep_lebensz_en.dta")) # importing the data
unlink(temp) # deleting temporary file

## Exploring the dataset

# How many unique cases do we have? As we can see below, we have 3,550 distinct cases in the dataset

soep %>% select(id) %>% unique() %>% count() 

# How many observation do we have by year?

soep %>% summarise(n = n(), .by = year) %>% arrange(year)

## Working with the most recent year

soep_last_year <- soep %>% 
  filter(year == max(soep$year, rm = TRUE))

# Proportion of female in the subset: 54,2% of the cases in the subset for 2004 are female

soep_last_year %>% 
  mutate(sex = factor(sex, levels = c(0,1), labels = c("Male", "Female"))) %>% 
  .$sex %>% 
  table() %>% 
  prop.table()*100


# Average subjective health by sex: Female's subjective health, on average, is lower (3.39) than male's one (3.46)

soep_last_year %>% 
  mutate(sex = factor(sex, levels = c(0,1), labels = c("Male", "Female"))) %>% 
  summarise(
    mean = mean(health_org, na.rm = TRUE),
    .by = sex
  )

# Exercise 2 --------------------------------------------------------------

# Setting a function to download more than one country and handle it as a dataframe

HMD_countries <- function(countries = country, HMD_username, HMD_password){
  # error messages
  if(is.null(HMD_username)) stop("You need to set a HMD username first in the argument 'HMD_username'...")
  if(is.null(HMD_password)) stop("You need to set a HMD password first in the argument 'HMD_password'...")
  
  # running the function to import for each country
  for(i in seq_along(countries)){
    # set a country in a list of countries
    cnt <- countries[i]
    # importing data from HMD website
    df <- HMDHFDplus::readHMDweb(
      paste0(cnt),
      item = "E0per",
      username = Sys.getenv("HMFD_user"),
      password = Sys.getenv("HMFD_password")
    )
    
    # creating column for country
    
    df <- df %>% 
      mutate(country = paste0(cnt))
    
    if(i == 1){
      df_countries <- df
    } else{
      df_countries <- df_countries %>% 
        bind_rows(df)
    }
    
    # next loop...
    rm(df)
    paste0("Finished for ", cnt,"...")
    
  }
  return(df_countries)
}

# Choosing countries to work with in a comparative way
country <- c("DNK","EST","ITA","CHL")

# Importing the data

df_countries <- HMD_countries(
  countries = country,
  HMD_username = Sys.getenv("HMFD_user"),
  HMD_password = Sys.getenv("HMFD_password")
)

# Visualizing trends of life expectancy at birth

df_countries %>% 
  # Filtering Year for the second half of 19th century
  filter(Year >= 1950) %>%
  # pivoting data for a column of sex labels and another for life expectancy at birth values
  pivot_longer(
    Female:Male,
    names_to = "sex",
    values_to = "e0"
  ) %>%
  # transforming sex and country as a factor
  mutate(
    sex = as.factor(sex),
    country = factor(country, levels = c("DNK","EST","ITA","CHL"), labels = c("Denmark","Estonia","Italy","Chile"))
  ) %>% 
  # Plotting data
  ggplot() +
  aes(x = Year, y = e0, color = country, group = country) +
  geom_line(linewidth = 1.02) +
  facet_wrap(. ~ sex) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1950,2030,10)) +
  scale_y_continuous(breaks = seq(50,90,5)) +
  labs(
    title = "Life Expectancy at birth (e0) over time by selected countries - 1950-2023",
    x = "Year",
    y = "Life Expectancy at Birth (e0)",
    color = "Selected Countries",
    caption = "Source: Human Mortality Database (HMD)"
  ) +
  scale_color_brewer(type = "qual",palette = "Dark2") +
  theme(
    plot.title = element_text(face = "bold",size = 16, hjust = 0, vjust = .5),
    plot.caption = element_text(face = "bold",size = 10, hjust = 1, vjust = .5),
    axis.title = element_text(colour = "#636363",face = "bold",size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(colour = "#636363",size = 11, hjust = .5, vjust = .5),
    strip.text = element_text(colour = "#636363",face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.title = element_blank(),
    legend.text = element_text(colour = "#636363",face = "bold", size = 10, hjust = .5, vjust = .5),
    legend.position = "top"
  )

# Visualizing gender gap over time among selected countries

df_countries %>% 
  # Filtering Year for the second half of 19th century
  filter(Year >= 1950) %>%
  # creating variable for gender gap (e0 female - e0 male)
  mutate(
    gender_gap = Female-Male
  ) %>% 
  # transforming country as a factor
  mutate(
    country = factor(country, levels = c("DNK","EST","ITA","CHL"), labels = c("Denmark","Estonia","Italy","Chile"))
  ) %>% 
  # Plotting data
  ggplot() +
  aes(x = Year, y = gender_gap, color = country, group = country) +
  geom_line(linewidth = 1.02) +
  theme_bw() +
  coord_cartesian(ylim = c(0,16)) +
  scale_x_continuous(breaks = seq(1950,2030,10)) +
  scale_y_continuous(breaks = seq(0,16,2)) +
  labs(
    title = "Gender gap in Life Expectancy at birth (Female - Male) over time by selected countries - 1950-2023",
    x = "Year",
    y = "Gender Gap in Life Expectancy at Birth (in years)",
    color = "Selected Countries",
    caption = "Source: Human Mortality Database (HMD)"
  ) +
  scale_color_brewer(type = "qual",palette = "Dark2") +
  theme(
    plot.title = element_text(face = "bold",size = 16, hjust = 0, vjust = .5),
    plot.caption = element_text(face = "bold",size = 10, hjust = 1, vjust = .5),
    axis.title = element_text(colour = "#636363",face = "bold",size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(colour = "#636363",size = 11, hjust = .5, vjust = .5),
    strip.text = element_text(colour = "#636363",face = "bold", size = 12, hjust = .5, vjust = .5),
    legend.title = element_blank(),
    legend.text = element_text(colour = "#636363",face = "bold", size = 10, hjust = .5, vjust = .5),
    legend.position = "top"
  )
