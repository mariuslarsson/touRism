#' Estimate the Gini Coefficient
#'
#' Gini coefficient estimated using the covariance approach, i.e.
#' \deqn{G_y = \frac{2 cov (y,F(y))}{\mu_y}}.
#' See Lerman and Yitzaki (1989) and A. Fernandez-Morales et al. (2016)
#'
#' @param x numeric vector
#'
#' @return value between 0 and (n-1)/n
#' @export
#'
#' @examples
#'
#' x <- c(1:12)
#' gini(x)
gini <- function(x){
  #Gini coefficient estimated using the covariance approach
  #See Lerman and Yitzaki (1989) and A. Fernandez-Morales et al. (2016)

  if(!is.numeric(x)) stop("x is not numeric")

  x <- sort(x)
  x_mean <- mean(x)
  x_sub <- x-x_mean

  Fy <- Frank(x)
  Fy_mean <- mean(Fy)
  Fy_sub <- Fy-Fy_mean
  wi <- 1/length(x)

  G <- (2*sum(x_sub*Fy_sub)*wi)/x_mean

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
#' #SOON
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
#' #SOON
giniRME <- function(y, x){

  S_x <- mean(x)/mean(y)
  gamma_xy <- giniCor(y, x)
  gini_x <- gini(x)
  gini_y <- gini(y)

  RME <- S_x * gamma_xy * gini_x/gini_y - S_x

  return(RME)
}

giniCov <- function(y, x){
  y_sub <- y - mean(y)
  x_sub <- x - mean(x)
  gcov <- sum(y_sub*x_sub)*(1/length(y))

  return(gcov)
}


Frank <- function(x){

  Fx <- rank(x)/length(x)

  return(Fx)
}
