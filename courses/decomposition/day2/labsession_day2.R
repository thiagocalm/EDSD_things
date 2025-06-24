options(scipen = 999999)
rm(list = ls())
invisible(gc())
# Packages ----------------------------------------------------------------

library(pacman)
p_load(tidyverse, shapley)

# Importing data ----------------------------------------------------------

children <- data.frame(
  age = c("20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44"),
  n1 = c(27, 152, 224, 239, 211),
  rate1 = c(37.037, 19.079, 15.179, 5.021, 6.161),
  n2 = c(363, 208, 96, 59, 48),
  rate2 = c(90.083, 76.923, 56.250, 20.339, 10.417)
)


# Example - gloves --------------------------------------------------------

# shapley function

glove <- function(factors) {
  if (length(factors) > 1 & 3 %in% factors) return (1)
  return (0)
}
# applying for this situation
shapley(glove, c(1, 2, 3), silent = TRUE)
