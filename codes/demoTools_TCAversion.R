#'-----------------------------------------------------------------------
#'@project EDSD courses
#'@date 2025-02-14
#'@code Function to calculate Life Table columns
#'@author Thiago Cordeiro Almeida
#'-----------------------------------------------------------------------

# Creating function -------------------------------------------------------

LT_function <- function(x, nDx = NULL, nNx = NULL, nMx = NULL, lx_radix = 100000){
  if(is.null(x)){
    stop("You have to inser x (age group)...")
  }
  if(is.null(nMx)){
    if(is.null(nDx) | is.null(nNx)){
      stop("You have to insert whether nMx or nDx/nNx...")
    } else{
      # nmx
      
      nmx <- nDx/nNx
    }
  }
  # n - age width
  
  n <- ifelse(x == 0, 1, ifelse(x == 1,4,ifelse(x == 85, NA, 5)))
  
  # nax - using Coale and Demeny (1983) equations
  
  nax <- ifelse(
    # Clause = age group 0-1
    x == 0 & nmx[1] >= .107, .350,
    ifelse(
      x == 0 & nmx[1] < .107, .053 + 2.8 * nmx[1],
      # Clause = age group 1-4
      ifelse(
        x == 1 & nmx[1] >= .107, 1.361,
        ifelse(
          x == 1 & nmx[1] < .107, 1.522 - 1.518 * nmx[1],
          # Clause = opened group - 85+
          ifelse(
            x == 85, 1/nmx,
            n/2
          )
        )
      )
    )
  )
  
  # nqx - using box 3.1 of PHG book
  
  nqx <- ifelse(
    x == 85, 1,
    (n * nmx)/(1 + (n-nax) * nmx)
  )
  
  # npx - using box 3.1 of PHG book
  
  npx <- 1-nqx
  
  # lx - using box 3.1 of PHG book
  
  lx <- NA
  
  for(i in seq_along(x)){
    if(i == 1){
      lx[i] <- lx_radix
    } else{
      lx[i] <- lx[i-1] * npx[i-1]
    }
  }
  
  # ndx - using box 3.1 of PHG book
  
  ndx <- lx * nqx
  
  # nLx - using box 3.1 of PHG book
  
  nLx <- ifelse(
    x == 85,
    lx/nmx,
    n * (lx - ndx) + nax * ndx
  )
  
  # Tx - using box 3.1 of PHG book
  
  Tx <- rev(cumsum(rev(nLx)))
  
  # ex - using box 3.1 of PHG book
  
  ex <- Tx/lx
  
  # Rounding decimals
  df <- data.frame(
    x = x,
    nNx = nNx,
    nDx = nDx,
    nmx = round(nmx,6),
    n = n,
    nax = round(nax,3),
    nqx = round(nqx,6),
    npx = round(npx,6),
    lx = round(lx,0),
    ndx = round(ndx,0),
    nLx = round(nLx,0),
    Tx = round(Tx,0),
    ex = round(ex,3)
  )
  
  # output
  
  return(df)
}


# Stable Population - Intrinsic R -----------------------------------------

StablePop_intrinsicR <- function(
    x = NULL,
    pop = NULL,
    births = NULL,
    nLx = NULL,
    nffx = NULL,
    T_start = 27.5,
    lx_radix = 100000,
    max_r_trials = NULL,
    digit_r_intrinsic = 5
){
  ## Handling data
  
  # Computing Female Fertility
  
  if(is.null(nffx)){
    nffx <- births/pop
    # dealing with NA values
    nffx[is.na(nffx)] <- 0
  }
  
  # Adjusts in nLx based on radix of lifetable
  
  if(max(nLx) > 5){
    nLx <- nLx/lx_radix
  }
  
  # Computing NRR
  
  NRR_start <- sum(nffx * nLx)
  
  # Computing intrinsic R
  
  r <- log(NRR_start)/T_start
  r_trials <- c(r)
  
  r_dev <- round(sum(exp(-r * (x + 2.5)) * (nffx * nLx)),5)
  
  while(round(r_dev,digit_r_intrinsic) != 1){
    # new intrinsic r
    r <- r + ((r_dev-1)/T_start)
    r_trials <- c(r_trials,r)
    
    r_dev <- round(sum(exp(-r * (x + 2.5)) * (nffx * nLx)),5)
  }
  
  # Defining Intrinsic R
  
  if(is.null(max_r_trials)){
    r_intrinsic <- tail(r_trials,n = 1)
  } else{
    r_intrinsic <- r_trials[max_r_trials]
  }
  
  
  # Output
  return(r_intrinsic)
}
