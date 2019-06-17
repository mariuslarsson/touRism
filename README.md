
# Welcome to touRism

This R package provides tools for analysis of tourism demand, especially
the analysis of seasonal concentration on tourism demand using
decomposition methods.

## Installation

Currently not on CRAN. ETA: summer 2019

Development version:

``` r
# install.packages("devtools")
devtools::install_github("mariuslarsson/touRism")
```

## Usage

``` r
library(touRism)
library(dplyr)
library(tidyr)

data(norwayGuestNights2018)
#?norwayGuestNights2018

norwayGuestNights2018 %>% 
  dplyr::mutate(Norway=rowSums(select_if(., is.numeric))) %>% 
  tidyr::gather(county, value, -month, -Norway) %>%
  dplyr::group_by(county) %>% 
  dplyr::summarise(gini = touRism::gini(value),
                   giniRME = touRism::giniRME(Norway, value)) %>% 
  dplyr::arrange(dplyr::desc(gini))
```

    ## # A tibble: 19 x 3
    ##    county                gini  giniRME
    ##    <chr>                <dbl>    <dbl>
    ##  1 Sogn.og.Fjordane     0.518  0.0377 
    ##  2 Møre.og.Romsdal      0.425  0.0238 
    ##  3 Aust.Agder           0.402  0.00478
    ##  4 Vest.Agder           0.388  0.00939
    ##  5 Nordland             0.362  0.0169 
    ##  6 Finnmark...Finnmárku 0.358  0.00733
    ##  7 Hordaland            0.344  0.0356 
    ##  8 Rogaland             0.320  0.00840
    ##  9 Akershus             0.314  0.00951
    ## 10 Telemark             0.312  0.00375
    ## 11 Vestfold             0.304  0.00199
    ## 12 Oppland              0.291 -0.00163
    ## 13 Trondelag            0.271  0.00258
    ## 14 Østfold              0.241 -0.00232
    ## 15 Buskerud             0.221 -0.0200 
    ## 16 Svalbard             0.205 -0.00466
    ## 17 Hedmark              0.193 -0.0297 
    ## 18 Oslo                 0.191 -0.0658 
    ## 19 Troms...Romsa        0.171 -0.0377

## Contact

<marius.larsson@gmail.com>
