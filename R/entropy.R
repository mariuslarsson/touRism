# theil <- function(x){
#
#   if(any(x==0)) warning("Zero observations will affect calulations")
#
#   x <- x[!x==0]
#   X <- sum(x)
#   p_t <- x/X
#   #p_t[is.nan(p_t)] <- 0
#   log_p_t <- log(1/p_t)
#   #log_p_t[is.infinite(log_p_t)] <- 0
#   #log_p_t[is.nan(log_p_t)] <- 0
#
#   #H <- -sum(p_t * log_p_t)
#   H <- sum(p_t * log_p_t)
#
#   log(length(x))-H
# }
# #theil(dirr)
#
