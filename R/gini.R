#' Estimate the Gini Coefficient
#'
#' Gini coefficient estimated using the covariance approach, i.e.
#' \deqn{G_y = \frac{2 cov (y,F(y))}{\mu_y}}.
#' See Lerman and Yitzaki (1985), Podder (1993) and A. Fernandez-Morales et al. (2016)
#'
#' Currently returns 0 if x is only zeroes.
#'
#' @param x numeric vector
#' @param suppressWarning logical. Set \code{TRUE} to hide \code{length(x) != 12} warning
#'
#' @return value between 0 and (n-1)/n
#' @export
#'
#' @examples
#'
#' x <- c(1:12)
#' gini(x)
gini <- function(x, suppressWarning = FALSE){

  #Error handling:
  if(!is.numeric(x)) stop("x is not numeric")

  if(!suppressWarning){
    if(length(x) != 12) warning("length of x is not equal to the number of months in a year")
  }

  if(sum(x)==0){
    return(0)
  }

  #Sort not needed.
  #x <- sort(x)
  x_mean <- mean(x)
  x_diff <- x-x_mean

  Fy <- Frank(x)
  Fy_mean <- mean(Fy)
  Fy_diff <- Fy-Fy_mean
  wi <- 1/length(x)

  COVyFy <- sum(x_diff*Fy_diff)*wi

  G <- (2*COVyFy)/x_mean

  return(G)
}

simplifiedGini <- function(x){
  wi <- 1/length(x)
  2*cov(x, Frank(x))*(1-wi)/(mean(x))
}


#' Estimate the Gini correlation
#'
#' @param y numeric vector
#' @param x numeric vector
#'
#' @return value betweel -1 and 1
#' @export
#'
#' @examples
#' x1 <- c(0,0,1,1,1,1.2,1.2,1.2,1,1,1,0)
#' x2 <- c(0.4,0.4,0.4,0,0,0,0,0,0,0,0,0.4)
#' y <- x1+x2
#'
#' giniCor(y, x1)
giniCor <- function(y,x){

  #Error handling:
  if(!is.numeric(x) | !is.numeric(y)) stop("x and/or y is not numeric")

  #Equal length requirement of x and y is handled by giniCov()

  gamma <- giniCov(x, Frank(y))/
    giniCov(x, Frank(x))

  return(gamma)
}

#' Estimate the RME
#'
#' RME measures the marginal effect each G_k has on G_y where sum k = 1 to n (G_k) = G_y
#'
#' @param y numeric vector
#' @param x numeric vector
#'
#' @return numeric
#' @export
#'
#' @examples
#' x1 <- c(0,0,1,1,1,1.2,1.2,1.2,1,1,1,0)
#' x2 <- c(0.4,0.4,0.4,0,0,0,0,0,0,0,0,0.4)
#' y <- x1+x2
#'
#' giniRME(y, x1)
giniRME <- function(y, x){

  S_x <- mean(x)/mean(y)
  gamma_xy <- giniCor(y, x)
  gini_x <- gini(x)
  gini_y <- gini(y)

  #C_x = S_x * gamma_xy as per Podder (1993)

  GRME <- S_x * gamma_xy * gini_x/gini_y - S_x

  return(GRME)
}

giniCov <- function(y, x){

  #Error handling:
  if(length(y) != length(x)) stop("length of x and y must be equal")

  y_diff <- y - mean(y)
  x_diff <- x - mean(x)
  Gcov <- sum(y_diff*x_diff)*(1/length(y))

  return(Gcov)
}


Frank <- function(x){

  Fx <- rank(x)/length(x)

  return(Fx)
}


#' Calculate months equivalent degree of tourism seasonality
#'
#' \deqn{12 \times (1-G_y)}
#' See Sitouras (2004).
#'
#' @param gini numeric value between 0 and (n-1)/n
#'
#' @return numeric value between 0 and 12
#' @export
#'
#' @examples
#'
#' x <- c(1:12)
#' giniMonths(gini(x))
giniMonths <- function(gini){

  n <- 12*(1-gini)

  return(n)
}
