#' Estimate the Gini Coefficient
#'
#' Gini coefficient estimated using the covariance approach
#' See Lerman and Yitzaki (1989) and A. Fernandez-Morales et al. (2016)
#'
#' @param x numeric vector
#'
#' @return value between 0 and (n-1)/n
#' @export
#'
#' @examples
#' #SOON
gini <- function(x){


  x <- sort(x)
  xmean <- mean(x)
  LHS <- x-xmean

  Fy <- Frank(x)
  Fmean <- mean(Fy)
  RHS <- sort(Fy)-Fmean
  wi <- 1/length(x)

  (2*sum(LHS*RHS)*wi)/xmean
}

#' Estimate the Gini correlation
#'
#' @param x1 numeric vector
#' @param x2 numeric vector
#'
#' @return value betweel -1 and 1
#' @export
#'
#' @examples
#' #SOON
giniCor <- function(x1,x2){
  giniCov((x2), Frank(x1))/
    giniCov((x2), Frank(x2))
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
RME <- function(y, x){

  S_x <- mean(x)/mean(y)
  gamma_xy <- giniCor(y, x)
  gini_x <- gini(x)
  gini_y <- gini(y)

  RME_x <- S_x * gamma_xy * gini_x/gini_y - S_x

  return(RME_x)
}

giniCov <- function(x1, x2){
  LHS <- x1 - mean(x1)
  RHS <- x2 - mean(x2)
  sum(LHS*RHS)*(1/length(x1))
}


Frank <- function(x){
  #Empirical distribution function F(y)
  rank(x)/length(x)
}
