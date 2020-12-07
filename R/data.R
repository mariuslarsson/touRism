#' Cruise passengers to Tromsø harbour
#'
#' A time series of cruise passengers to Tromsø harbour
#' May 2015 - May 2019.
#'
#' @format A data frame with 49 rows and 2 variables:
#' \describe{
#'   \item{date_start}{Month of observation}
#'   \item{Passasjerer}{Number of passengers}
#' }
#' @source \url{https://www.kystverket.no/safeseanet}
"tromso"

#' Guest nights Norway 2018
#'
#' Guest nights in Norway for the year 2018 by county
#'
#'
#' @format A data frame with 12 rows and 20 variables:
#' \describe{
#'   \item{month}{Month of observation}
#'   \item{LOCATIONS}{Counties in Norway}
#'   \item{...}{...}
#' }
#' @source \url{https://www.ssb.no}
"norwayGuestNights2018"

#' Exchange rates
#'
#' Exchange rates for 20 currencies against the NOK, 2007-2020
#'
#'
#' @format A tibble with 167 rows and 22 columns
#' \describe{
#'   \item{date_start}{Month of observation}
#'   \item{year}{Year of observation}
#'   \item{...}{Currency}
#' }
#' @source \url{https://www.norges-bank.no}
"laspeyresCurrencies"

#' Yearly guest night weights
#'
#' Yearly guest nights weights by country of origin to Norway, 2006-2020
#'
#'
#' @format A tibble with 15 rows and 21 columns:
#' \describe{
#'   \item{year}{Year of observation}
#'   \item{...}{country of origin}
#' }
#' @source \url{https://www.ssb.no}
"laspeyresWeights"



