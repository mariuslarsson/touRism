#' Function to generate chained Laspeyres index
#'
#' EXPECT MAJOR CHANGES
#'
#' @param weights tibble with yearly weights where each row is a year and each column is the weight of a given country. The column names of the country weights must be equal to those of the exchangerates column names. In addition, a coulmn with years must be included, named year
#' @param exchangerates tibble with monthly exchange rates where each row is a month and each column is the exchange rate of a currency. The column names of the currencies must be equal to those of the weights column names. In addition, a coulmn with months must be included, named date_start.
#'
#' @return tibble with dates and index
#' @export
#'
#' @examples
#'
#' #Soon
chainedLaspeyres <- function(weights, exchangerates){



  laggedWeights <- weights %>%
    mutate_at(vars(-year), function(x) dplyr::lag(x)) %>%
    filter(year != min(year)) %>%
    mutate(year = as.character(year))


  exchangeratesYear <- exchangerates %>%
    mutate(year = format(as.Date(date_start), "%Y"))

  #Generate yearIndex formula for parsing in mutate:
  #prod(yearIndecies_I)*100
  yearIndexFormula <- exchangeratesYear %>%
    select(-c(date_start, year)) %>%
    colnames() %>%
    paste0("yearIndecies", ., collapse="*") %>%
    paste0("*100")


  joinedWeightsExchangerates <- exchangeratesYear %>%
    pivot_longer(-c(date_start, year),values_to = "currencies") %>%
    left_join(laggedWeights %>%
                pivot_longer(-c(year),values_to = "weights"), by=c("year", "name")) #%>%
  #mutate(weights = ifelse(is.na(weights), 0, weights))

  uniqueDates <- unique(joinedWeightsExchangerates$date_start)
  numUniqueGroups <- ceiling(length(uniqueDates)/12)
  uniqueGroups <- sort(rep(paste0("group", 1:numUniqueGroups), 12))[seq_along(uniqueDates)]
  firstMonth <- format(min(joinedWeightsExchangerates$date_start), "%m")

  dualTempYearlyIndecies <- joinedWeightsExchangerates %>%
    group_by(name) %>%
    #filter(name == "USD") %>%
    mutate(group = uniqueGroups) %>%
    #Generating support variables to estimate overlapping yearly indecies
    mutate(finalPeriodWeight = lag(ifelse(format(date_start, "%m")==firstMonth, weights, NA), 12),
           finalPeriodFirstCurrency = lag(ifelse(format(date_start, "%m")==firstMonth, currencies, NA), 12)) %>%
    #finalPeriodWeight = lag(finalPeriodWeight, 12)
    group_by(group, name) %>%
    #Laspeyres index for the overlapping period
    mutate(yearIndeciesNew = (currencies/finalPeriodFirstCurrency)^first(finalPeriodWeight)) %>%
    select(-c(finalPeriodWeight, finalPeriodFirstCurrency)) %>%
    mutate(yearIndecies = (currencies/first(currencies))^first(weights)) %>%
    ungroup()

  fullYearIndex <- dualTempYearlyIndecies %>%
    select(-yearIndeciesNew) %>%
    pivot_wider(-c(currencies, weights), names_from = name, values_from = yearIndecies, names_prefix = "yearIndecies") %>%
    mutate(yearIndex = eval(parse(text=yearIndexFormula))) %>%
    select(date_start, group, yearIndex)

  dualYearIndex <- dualTempYearlyIndecies %>%
    select(-yearIndecies) %>%
    pivot_wider(-c(currencies, weights), names_from = name, values_from = yearIndeciesNew, names_prefix = "yearIndecies") %>%
    mutate(yearIndexOverlap = eval(parse(text=yearIndexFormula))) %>%
    select(date_start, group, yearIndexOverlap)



  ######################################################################################################
  #dff <- data.frame(0)

  testFrame <- fullYearIndex %>%
    left_join(dualYearIndex, by=c("date_start", "group")) %>%
    mutate(yearIndexOverlap= lead(yearIndexOverlap)) %>%
    as.data.frame() %>%
    mutate(chainedIndex = NA)

  for(i in 1:(numUniqueGroups)){
    #i <- 1
    if(i == 1){
      indexVecFirst <- testFrame %>%
        filter(group==unique(uniqueGroups)[i]) %>%
        .$yearIndex
      indexOverlapFirst <- testFrame %>%
        filter(group==unique(uniqueGroups)[i]) %>%
        filter(!is.na(yearIndexOverlap)) %>%
        .$yearIndexOverlap

      tempIndexVec <- c(indexVecFirst, indexOverlapFirst, rep(NA, nrow(testFrame)-length(indexVecFirst)-1))

      testFrame[[paste0("de", i)]] <- tempIndexVec

      k <- length(tempIndexVec[!is.na(tempIndexVec)])

      testFrame[["chainedIndex"]] <- tempIndexVec
      #i <- 2
    } else {

      newVariable <- paste0("de", i)
      prevVariable <- paste0("de", i-1)
      indexOverlapFirst <- testFrame %>%
        filter(group==unique(uniqueGroups)[i]) %>%
        filter(!is.na(eval(parse(text=prevVariable)))) %>%
        .[[prevVariable]]

      indexVec <- testFrame %>%
        filter(group==unique(uniqueGroups)[i]) %>%
        .$yearIndex
      indexOverlap <- testFrame %>%
        filter(group==unique(uniqueGroups)[i]) %>%
        filter(!is.na(yearIndexOverlap)) %>%
        .$yearIndexOverlap

      partialChain <- c(indexVec, indexOverlap)*indexOverlapFirst/100

      testFrame[k:(k+length(partialChain)-1),newVariable] <- partialChain
      testFrame[k:(k+length(partialChain)-1),"chainedIndex"] <- partialChain
      k <- k+length(partialChain)-1

    }



  }

  outputIndex <- testFrame %>%
    select(date_start, chainedIndex) %>%
    as_tibble()

  return(outputIndex)

}
