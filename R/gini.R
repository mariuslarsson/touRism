#' Estimate the Gini Coefficient
#'
#' Gini coefficient estimated using the covariance approach, i.e.
#' \deqn{G_y = \frac{2 cov (y,F(y))}{\mu_y}}.
#' See Lerman and Yitzaki (1989) and A. Fernandez-Morales et al. (2016)
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
    if(length(x) != 12) warning("length of x is different than the number of months in a year")
  }

  x <- sort(x)
  x_mean <- mean(x)
  x_diff <- x-x_mean

  Fy <- Frank(x)
  Fy_mean <- mean(Fy)
  Fy_diff <- Fy-Fy_mean
  wi <- 1/length(x)

  G <- (2*sum(x_diff*Fy_diff)*wi)/x_mean

  return(G)
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
#' giniCor(y, x1)
giniRME <- function(y, x){

  S_x <- mean(x)/mean(y)
  gamma_xy <- giniCor(y, x)
  gini_x <- gini(x)
  gini_y <- gini(y)

  RME <- S_x * gamma_xy * gini_x/gini_y - S_x

  return(RME)
}

giniCov <- function(y, x){
  y_diff <- y - mean(y)
  x_diff <- x - mean(x)
  gcov <- sum(y_diff*x_diff)*(1/length(y))

  return(gcov)
}


Frank <- function(x){

  Fx <- rank(x)/length(x)

  return(Fx)
}
