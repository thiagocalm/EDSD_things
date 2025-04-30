#'---------------------------------------
#'@date 2025-04-30
#'@program EDSD
#'@course Population Projections
#'@professor Marilia Nepomuceno (MPIDR)
#'@description Course's exercises - day 3
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer


# libraries ---------------------------------------------------------------

library(tidyverse)


# Importing data ----------------------------------------------------------

load("data/EDSD.lecture2.Rdata")



# Leslei matrix -----------------------------------------------------------

# Empty matrix
L_matrix <- matrix(data = 0, nrow = nrow(dta.swe), ncol = nrow(dta.swe))

# adding fertility data

dta.swe$bFx <- ifelse(is.na(dta.swe$bFx), 0, dta.swe$bFx)

L_matrix[1,] <- dta.swe$bFx

# adding mortality data
diag(L_matrix[-1,]) <- dta.swe$SFx[-length(dta.swe$SFx)]

# adding last age group for mortality

L_matrix[length(dta.swe$SFx),length(dta.swe$SFx)] <- dta.swe$SFx[length(dta.swe$SFx)-1]

# Projecting the population one interval further

Lfx5.matrix <- L_matrix %*% dta.swe$NFx

# function for automatizing it --------------------------------------------

PopProjLeslei <- function(Nbase_Fem,Nbase_Male = NULL, Sx_Fem,Sx_Male = NULL, bFx,bMx = NULL, age_groups, interval_proj){
  # parameters
  age_group_range = age_groups[3]-age_groups[2]
  proj_periods = interval_proj/age_group_range
  dim = dim(age_groups)

  if(!is.null(Nbase_Male)){
    L <- matrix(data = 0, nrow = dim*2, ncol = dim*2)
  } else{
    L <- matrix(data = 0, nrow = dim, ncol = dim)
  }

  # adding fertility data

  bFx <- ifelse(is.na(bFx), 0, bFx)

  L[1,] <- bFx

  # adding mortality data

  if(!is.null(Nbase_Male)){
    diag(L[-1,]) <- cbind(Sx_Fem[-dim],Sx_Male[-dim])
  } else{
    diag(L[-1,]) <- Sx[-dim]
  }

  # adding last age group for mortality

  if(!is.null(Nbase_Male)){
    diag(L[-1,]) <- cbind(Sx_Fem[-dim],Sx_Male[-dim])
    L[dim,dim] <- Sx_Fem[dim-1] # female last mortality survivorship group
    L[dim*2,dim*2] <- Sx_Male[dim-1] # male last mortality survivorship group
  } else{
    L[dim,dim] <- Sx[dim-1]
  }

  # Projecting the population one interval further

  Proj_pop = matrix(data = 0, nrow = dim*2, ncol = proj_periods+1)

  if(!is.null(Nbase_Male)){
    Proj_pop[,1] = cbind(Nbase_Fem,Nbase_Male)
  } else{
    Proj_pop[,1] = Nbase_Fem
  }

  for(t in 2:proj_periods){
    Proj_pop[t] <- L %*% Proj_pop[t-1]
  }

  out <- cbind(data.frame(x = age_groups, sex = rep("Female",dim) Proj_pop))
  return(out)

}

# projecting this pop

PopProj_100 <- PopProjLeslei(
  Nbase = dta.swe$NFx,
  Sx = dta.swe$SFx,
  bFx = dta.swe$bFx,
  age_groups = dta.swe$Age,
  interval_proj = 100
)
