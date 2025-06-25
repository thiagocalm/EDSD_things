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


# Example - Kitagawa's decomp ---------------------------------------------

# Shapley function
rates <- function(factors, data) {
  if ("weight" %in% factors) {
    weights <- data$n2 / sum(data$n2)
  } else {
    weights <- data$n1 / sum(data$n1)
  }
  if ("rate" %in% factors) {
    rate <- data$rate2
  } else {
    rate <- data$rate1
  }
  return(sum(weights * rate))
}

# each factor in our dimenion
rates(c(), children)
rates(c("weight"), children)
rates(c("rate"), children)
rates(c("weight", "rate"), children)

# Shapley decomposition
shapley(
  rates,
  c("weight", "rate"),
  silent = TRUE,
  data = children
)


# Example - Bongaarts decomposition ---------------------------------------

# data
bongaarts <- data.frame(
  factor = c("fecundity", "marriage", "noncontracept", "abortion", "lactation"),
  `1960` = c(16.158, 0.720, 0.970, 0.970, 0.560),
  `1970` = c(16.573, 0.580, 0.760, 0.840, 0.660)
)

bongaarts

# way to define it

tfr2 = bongaarts$X1970[1] * bongaarts$X1970[2] * bongaarts$X1970[3] * bongaarts$X1970[4] * bongaarts$X1970[5]
tfr1 = bongaarts$X1960[1] * bongaarts$X1960[2] * bongaarts$X1960[3] * bongaarts$X1960[4] * bongaarts$X1960[5]
tfr2
tfr1
dif = tfr2-tfr1
dif

# value function

bongaarts_vf <- function(factors = c(),data){
  pro_mar <- ifelse(identical(factors,"marriage"),data$X1970[2],data$X1960[2])
  pro_noncont <- ifelse(identical(factors,"noncontraception"),data$X1970[3],data$X1960[3])
  pro_abortion <- ifelse(identical(factors,"abortion"),data$X1970[4],data$X1960[4])
  pro_lactation <- ifelse(identical(factors,"lactation"),data$X1970[5],data$X1960[5])
  pro_fecundity <- ifelse(identical(factors,"fecundity"),data$X1970[1],data$X1960[1])

  # tfr
  tfr = pro_mar * pro_noncont * pro_abortion * pro_lactation * pro_fecundity
  return(tfr)
}


# testing it
bongaarts_vf(data = bongaarts) # it is working now!

bongaarts_vf(
  data = bongaarts,
  factors = c("marriage", "noncontraception", "abortion","lactation","fecundity")
)
# applying decomposition

shapley(bongaarts_vf,
        c("marriage", "noncontraception", "abortion","lactation","fecundity"),
        data = bongaarts,
        silent = TRUE)
