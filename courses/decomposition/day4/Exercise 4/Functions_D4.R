# Decomposition class - EDSD 2024
# FUNCTIONS AND INFORMATION FOR DAY 3

# Information for graphs
# Labels for causes of death
cause_names<-c("1"="Infectious and respiratory", "2"="Neoplasm","3"="Circulatory and heart",
               "4"="Birth","5"="Diabetes",
               "6"= "Homicide","7"="Other external",
               "8"="Other","9"="Undefined")

# Functions to use with DemoDecomp functions
# Life expectancy at birth
e0.frommx <- function(mx =  mx,age, sex=NULL, ax = NULL){
  n   <- c(diff(age))
  n <- c(n,n[length(n)])
  
  if (is.null(ax)) {
    ax <- 0.5 * n
    ax[length(ax)] <- if (mx[length(mx)] == 0) 0.5 else 1/mx[length(mx)]
    
    if(!is.null(sex)){
      if (n[2] == 4) {
        if (sex == 1) {
          if (mx[1] >= 0.107) {
            ax[1] <- 0.33
            ax[2] <- 1.352
          }
          else {
            ax[1] <- 0.045 + 2.684 * mx[1]
            ax[2] <- 1.651 - 2.816 * mx[1]
          }
        }
        if (sex == 2) {
          if (mx[1] >= 0.107) {
            ax[1] <- 0.35
            ax[2] <- 1.361
          }
          else {
            ax[1] <- 0.053 + 2.8 * mx[1]
            ax[2] <- 1.522 - 1.518 * mx[1]
          }
        }
      }
    }
  }
  
  qx          <- (n * mx)/(1 + (n - ax) * mx)
  qx          <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  
  px <- 1 - qx
  lx <- cumprod(c(1, px))
  dx <- -diff(lx)
  lxpn <- lx[-1]
  Lxpn <- n * lxpn + dx * ax
  Lx <- c(Lxpn[-length(Lxpn)], lxpn[length(lxpn)]/mx[length(mx)])
  Tx <- rev(cumsum(rev(Lx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  e0 <- ex[1]
  
  return(e0)
}


e0.frommxc <- function(mxcvec,age,sex=NULL,ax=NULL){
  dim(mxcvec) <- c(length(age),length(mxcvec)/length(age))
  mx          <- rowSums(mxcvec)
  e0.frommx(mx=mx,age=age,sex=sex,ax=NULL)
}


# Lifespan disparity
edagger.frommx <- function(mx =  mx,age, sex=NULL, ax = NULL){
  n   <- c(diff(age))
  n <- c(n,n[length(n)])
  
  if (is.null(ax)) {
    ax <- 0.5 * n
    ax[length(ax)] <- if (mx[length(mx)] == 0) 0.5 else 1/mx[length(mx)]
    
    if(!is.null(sex)){
      if (n[2] == 4) {
        if (sex == 1) {
          if (mx[1] >= 0.107) {
            ax[1] <- 0.33
            ax[2] <- 1.352
          }
          else {
            ax[1] <- 0.045 + 2.684 * mx[1]
            ax[2] <- 1.651 - 2.816 * mx[1]
          }
        }
        if (sex == 2) {
          if (mx[1] >= 0.107) {
            ax[1] <- 0.35
            ax[2] <- 1.361
          }
          else {
            ax[1] <- 0.053 + 2.8 * mx[1]
            ax[2] <- 1.522 - 1.518 * mx[1]
          }
        }
      }
    }
  }
  
  qx          <- (n * mx)/(1 + (n - ax) * mx)
  qx          <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  
  px <- 1 - qx
  lx <- cumprod(c(1, px))
  dx <- -diff(lx)
  lxpn <- lx[-1]
  Lxpn <- n * lxpn + dx * ax
  Lx <- c(Lxpn[-length(Lxpn)], lxpn[length(lxpn)]/mx[length(mx)])
  Tx <- rev(cumsum(rev(Lx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  ex[is.na(ex)] <- 0
  ex[length(ex)] <- if (ex[length(ex)] == 0) 0 else ax[length(ex)]
  
  v        <- (ax*c(ex[-1L],0) + (1-ax)*ex)
  v[length(ex)] <- ex[length(ex)]
  v <- dx*v
  e.dagger <- rev(cumsum(rev(v)))/lx
  
  return(e.dagger[1])
}


edagger.frommxc <- function(mxcvec,sex=NULL,age,ax=NULL){
  dim(mxcvec) <- c(length(age),length(mxcvec)/length(age))
  mx          <- rowSums(mxcvec)
  edagger.frommx(mx=mx,age=age)
}


# Standard deviation
sd.frommx <- function(mx =  mx,age, sex=NULL, ax = NULL){
  n   <- c(diff(age))
  n <- c(n,n[length(n)])
  
  if (is.null(ax)) {
    ax <- 0.5 * n
    ax[length(ax)] <- if (mx[length(mx)] == 0) 0.5 else 1/mx[length(mx)]
    
    if(!is.null(sex)){
      if (n[2] == 4) {
        if (sex == 1) {
          if (mx[1] >= 0.107) {
            ax[1] <- 0.33
            ax[2] <- 1.352
          }
          else {
            ax[1] <- 0.045 + 2.684 * mx[1]
            ax[2] <- 1.651 - 2.816 * mx[1]
          }
        }
        if (sex == 2) {
          if (mx[1] >= 0.107) {
            ax[1] <- 0.35
            ax[2] <- 1.361
          }
          else {
            ax[1] <- 0.053 + 2.8 * mx[1]
            ax[2] <- 1.522 - 1.518 * mx[1]
          }
        }
      }
    }
  }
  
  qx          <- (n * mx)/(1 + (n - ax) * mx)
  qx          <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  
  px <- 1 - qx
  lx <- cumprod(c(1, px))
  dx <- -diff(lx)
  lxpn <- lx[-1]
  Lxpn <- n * lxpn + dx * ax
  Lx <- c(Lxpn[-length(Lxpn)], lxpn[length(lxpn)]/mx[length(mx)])
  Tx <- rev(cumsum(rev(Lx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  ex[is.na(ex)] <- 0
  ex[length(ex)] <- if (ex[length(ex)] == 0) 0 else ax[length(ax)]
  # 
  sd <-  sqrt(sum(dx/lx[1]*(age + ax - ex[1])^2))
  
  return(sd)
}

sd.frommxc <- function(mxcvec,sex=NULL,age,ax=NULL){
  dim(mxcvec) <- c(length(age),length(mxcvec)/length(age))
  mx          <- rowSums(mxcvec)
  sd.frommx(mx,sex=NULL,age=age,ax=NULL)
}

# Relative Gini

# Gini function from PASH
Gini.fun <- function (x, ax, dx, ex) {
  e = rep(1, length(x))
  D = outer(dx, dx)
  x_ = x+ax
  X_ = abs(e%*%t(x_) - x_%*%t(e))
  G = sum(D*X_)/(2*ex[1L])
  return(g=G)
}


rG.frommx <- function(mx =  mx,age, sex=NULL, ax = NULL){
  n   <- c(diff(age))
  n <- c(n,n[length(n)])
  
  if (is.null(ax)) {
    ax <- 0.5 * n
    ax[length(ax)] <- if (mx[length(mx)] == 0) 0.5 else 1/mx[length(mx)]
    
    if(!is.null(sex)){
      if (n[2] == 4) {
        if (sex == 1) {
          if (mx[1] >= 0.107) {
            ax[1] <- 0.33
            ax[2] <- 1.352
          }
          else {
            ax[1] <- 0.045 + 2.684 * mx[1]
            ax[2] <- 1.651 - 2.816 * mx[1]
          }
        }
        if (sex == 2) {
          if (mx[1] >= 0.107) {
            ax[1] <- 0.35
            ax[2] <- 1.361
          }
          else {
            ax[1] <- 0.053 + 2.8 * mx[1]
            ax[2] <- 1.522 - 1.518 * mx[1]
          }
        }
      }
    }
  }
  
  qx          <- (n * mx)/(1 + (n - ax) * mx)
  qx          <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  
  px <- 1 - qx
  lx <- cumprod(c(1, px))
  dx <- -diff(lx)
  lxpn <- lx[-1]
  Lxpn <- n * lxpn + dx * ax
  Lx <- c(Lxpn[-length(Lxpn)], lxpn[length(lxpn)]/mx[length(mx)])
  Tx <- rev(cumsum(rev(Lx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  ex[is.na(ex)] <- 0
  ex[length(ex)] <- if (ex[length(ex)] == 0) 0 else ax[length(ax)]
  
  # 
  rG <- Gini.fun(x = age,ax = ax,dx = dx/100000,ex = ex)
  return(rG[1])
}

rG.frommxc <- function(mxcvec,sex=1,age,ax=NULL){
  dim(mxcvec) <- c(length(age),length(mxcvec)/length(age))
  mx          <- rowSums(mxcvec)
  rG.frommx(mx,sex,age=age,ax=ax)
}


# Continuous change decomposition algorithm (from DemoDecomp)

decomp_cont <- function (func, pars1, pars2, N, ...) 
{
  y1 <- edagger.frommxc(pars1, age=c(0,1,seq(5,85,5)))
  y2 <- edagger.frommxc(pars2, age=c(0,1,seq(5,85,5)))
  d <- pars2 - pars1 # difference between each parameter
  n <- length(pars1)
  
  delta <- d/N # how much of the difference we add at each iteration
  
  x <- pars1 + d * matrix(rep(0.5:(N - 0.5)/N, n), byrow = TRUE, # increase pars1 so it gets to the middle value of each iteration
                          ncol = N)
  cc <- matrix(0, nrow = n, ncol = N) # prepare results matrix
  zeros <- rep(0, n)
  for (j in 1:N) {
    DD <- diag(delta/2) # matrix to only increase the parameter we are interested in for that specific iteration
    for (i in 1:n) {
      # calculating what the difference would be between the two aggregate measures if
      # - the parameter of interested increased by d/N
      # - all other parameters were fixed at the midpoint of the iteration interval
      cc[i, j] <- edagger.frommxc((x[, j] + DD[, i]), age=c(0,1,seq(5,85,5))) - edagger.frommxc((x[,j] - DD[, i]), age=c(0,1,seq(5,85,5))) 
    }
  }
  return(rowSums(cc))
}



# Stepwise replacement decomposition algorithm (from DemoDecomp)

decomp_step <- function (func, pars1, pars2, symmetrical = TRUE, direction = "up", 
                         ...) 
{
  # Clean up
  direction <- tolower(direction)
  stopifnot(direction %in% c("up", "down", "both"))
  up <- direction %in% c("up", "both")
  down <- direction %in% c("down", "both")
  
  # Setup
  N <- length(pars1)
  # parameters for the two populations
  pars1Mat <- matrix(pars1, ncol = N + 1, nrow = N)
  pars2Mat <- matrix(pars2, ncol = N + 1, nrow = N)
  # Empty matrices for calculation
  RM_1_2_up <- matrix(ncol = N + 1, nrow = N)
  RM_1_2_down <- RM_1_2_up
  RM_2_1_up <- RM_1_2_up
  RM_2_1_down <- RM_1_2_up
  
  # Fill in the matrices for substitution from population 1 to population 2
  # Determine the right succession of parameters from population 1 and population 2 
  r1ind <- lower.tri(pars1Mat, TRUE)
  r2ind <- upper.tri(pars1Mat)
  
  RM_1_2_up[r1ind] <- pars1Mat[r1ind]  # Fill matrix' lower triangle with parameters from population 1
  RM_1_2_up[r2ind] <- pars2Mat[r2ind]  # Fill matrix' upper triangle with parameters from population 2
  
  RM_1_2_down[r1ind[N:1, ]] <- pars1Mat[r1ind[N:1, ]] # Invert the triangle for inverse direction
  RM_1_2_down[r2ind[N:1, ]] <- pars2Mat[r2ind[N:1, ]]
  
  # Fill in the matrices for substitution from population 2 to population 1
  RM_2_1_up[r1ind] <- pars2Mat[r1ind]
  RM_2_1_up[r2ind] <- pars1Mat[r2ind]
  RM_2_1_down[r1ind[N:1, ]] <- pars2Mat[r1ind[N:1, ]]
  RM_2_1_down[r2ind[N:1, ]] <- pars1Mat[r2ind[N:1, ]]
  
  # Empty results matrix
  dec <- matrix(NA, nrow = N, ncol = 4)
  
  # Substitute based on chosen settings
  if (up) {
    dec[, 1] <- diff(apply(RM_1_2_up, 2, func, ...))
  }
  if (down) {
    dec[, 2] <- diff(apply(RM_1_2_down, 2, func, ...))
  }
  if (symmetrical) {
    if (up) {
      dec[, 3] <- -diff(apply(RM_2_1_up, 2, func, ...))
    }
    if (down) {
      dec[, 4] <- -diff(apply(RM_2_1_down, 2, func, ...))
    }
  }
  dec_avg <- rowMeans(dec, na.rm = TRUE)
  dec_avg
}