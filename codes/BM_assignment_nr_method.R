#' EDSD
#' @course Basic Math
#' @activity Assignment 1
#' @place MPIDR
#' @date 2024-09-16
#' @goal Apply Newton-Raphson's method for a empirical situation
#' @description The empirical situation selected was the exercise 33 of chapter 4 (STEWART, 2003)
#' -----------------------------------------------------------------------------------------------

# Setting R configurations
options(scipen = 99999)
rm(list = ls())


# Creating Function for applying the method -------------------------------

nr_method <- function(x, f_x_func,d_fx_func){

  # Importing required packages
  require(tidyverse)
  require(patchwork)

  # seting some objects to save results and tries
  tries <- NULL
  results <- NULL

  # Creating objects
  tries <- c(tries,x)

  # Calculating our function and derivative
  f_x = f_x_func(x)
  d_fx = d_fx_func(x)

  result = x - f_x/d_fx

  results <- c(results,result)

  # assessment

  assessment <- round(x,8) == round(result,8)

  # Second try - repeat all the steps above

  while(assessment == FALSE){
    x = result
    tries <- c(tries,x)

    f_x = f_x_func(x)
    d_fx = d_fx_func(x)

    result = x - f_x/d_fx
    result

    results <- c(results,result)

    # assessment

    assessment <- round(x,8) == round(result,8)

  }

  # Creating plots to analyse results
  fig_tries <- ggplot() +
    aes(
      x = seq(1,length(tries)),
      y = tries
    ) +
    geom_line(size = 1, color = "#b35806") +
    geom_point(color = "#b35806") +
    labs(
      title = "Input values over tries",
      y = "Input values",
      x = "Tries"
    ) +
    scale_x_continuous(breaks = seq(0,length(tries),2)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20),
      axis.title = element_text(face = "bold", size = 14, vjust = .5, hjust = 1),
      axis.text = element_text(size = 11, vjust = .5, hjust = .5)
    )

  fig_tries_1 <- ggplot() +
    aes(
      x = seq(1,length(tries)-1),
      y = tries[2:length(tries)]
    ) +
    geom_line(size = 1, color = "#b35806") +
    geom_point(color = "#b35806") +
    labs(
      title = "Input values over tries (without 'guess' try)",
      y = "Input values",
      x = "Tries"
    ) +
    scale_x_continuous(breaks = seq(0,length(tries),2)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20),
      axis.title = element_text(face = "bold", size = 14, vjust = .5, hjust = 1),
      axis.text = element_text(size = 11, vjust = .5, hjust = .5)
    )

  fig_results <- ggplot() +
    aes(
      x = seq(1,length(results)),
      y = results
    ) +
    geom_line(size = 1,color = "#542788") +
    geom_point(color = "#542788") +
    labs(
      title = "Results over tries",
      y = "Results",
      x = "Tries"
    ) +
    scale_x_continuous(breaks = seq(0,length(results),2)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20),
      axis.title = element_text(face = "bold", size = 14, vjust = .5, hjust = 1),
      axis.text = element_text(size = 11, vjust = .5, hjust = .5)
    )

  fig_results_1 <- ggplot() +
    aes(
      x = seq(1,length(results)-1),
      y = results[2:length(results)]
    ) +
    geom_line(size = 1, color = "#542788") +
    geom_point(color = "#542788") +
    labs(
      title = "Results over tries (without 'guess' try)",
      y = "Results",
      x = "Tries"
    ) +
    scale_x_continuous(breaks = seq(0,length(tries),2)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20),
      axis.title = element_text(face = "bold", size = 14, vjust = .5, hjust = 1),
      axis.text = element_text(size = 11, vjust = .5, hjust = .5)
    )

  # Ordering plots
  fig_final_total <- (fig_tries / fig_results)
  fig_final_whitout1 <- (fig_tries / fig_results_1)

  # Output with results and plots
  output = list(
    results = tibble("tries" = tries, "results" = results),
    fig_final_total = fig_final_total,
    fig_final_whitout1 = fig_final_whitout1
  )

  return(output)
}


# Applying for some examples ----------------------------------------------

# Setting our functions

f_x_func = function(x){
  return(48*x * (1+x)^60 - (1+x)^60 + 1)
}
d_fx_func = function(x){
  return((1+x)^59 * (48*x*61-12))
}

# Generating the results - input = 1

result1 = nr_method(x = 1, f_x_func = f_x_func, d_fx_func = d_fx_func)

result1$results
result1$fig_final_total

# Generating the results - input = 2 and -2

result2 = nr_method(x = 2, f_x_func = f_x_func, d_fx_func = d_fx_func)
result_2 = nr_method(x = -2, f_x_func = f_x_func, d_fx_func = d_fx_func)

result2$results
result2$fig_final_total

result_2$results
result_2$fig_final_total

# Look what's happenning: the root that we find when use 2 and -2 are different!!
# It's happening because when we start with work 'guess', this kind of wrong root can be returned!
# Reference for that: https://personal.math.ubc.ca/~anstee/math104/104newtonmethod.pdf (topic 4.2)

# Generating the results

result0 = nr_method(x = .1, f_x_func = f_x_func, d_fx_func = d_fx_func)
result_0 = nr_method(x = -.1, f_x_func = f_x_func, d_fx_func = d_fx_func)

result0$results %>% slice(length(result0$results$tries))
result0$fig_final_total

result_0$results %>% slice(length(result_0$results$tries))
result_0$fig_final_total

# Example of the presentation ---------------------------------------------

# Imput functions
f_x_func = function(x) {-(1+x)^15 + 12*x * (1+x)^15 + 1}
d_fx_func = function(x) {(-15 * (1+x)^14) + (12 * (1+x)^15) + (12 * x * 15 * (1 + x)^14)}

# Guessing with 2
guess = 2
results2 <- nr_method(x = guess, f_x_func = f_x_func, d_fx_func = d_fx_func)
results2$results %>% View()
graf_1 <- results2$fig_final_total

# Guessing with 4
guess = 4
results4 <- nr_method(x = guess, f_x_func = f_x_func, d_fx_func = d_fx_func)
results4$results %>% View()
graf_2 <- results4$fig_final_total

# Guessing with very small and negative number
guess = 0.014
results0 <- nr_method(x = guess, f_x_func = f_x_func, d_fx_func = d_fx_func)
results0$results %>% View()
graf_3 <- results0$fig_final_total

# Guessing with very small and negative number
guess = -0.014
results_0 <- nr_method(x = guess, f_x_func = f_x_func, d_fx_func = d_fx_func)
results_0$results %>% View()
