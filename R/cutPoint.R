#' cut_Points
#'
#' @param data Must include, at minimun, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#'
#' @return filtered tibble
#' @export
#'
#' @examples test
cut_Points <- function(data) {
  
  library(tidyverse)
  library(magrittr)
  library(pracma)
  library(strucchange)
  
  data %>%
    filter(Cluster3 == "Dropped") %>%
    group_by(AccountNumber) %>%
    # calculate startDate #
    mutate(M_AVG = movavg(Count, 21, "s"),
           quant = quantile(M_AVG[M_AVG > 0], probs = 0.2),
           startDate = min(Date[(M_AVG > quant)])) %>%
    filter(Date >= startDate) %>%
    # calculate temporary endDate of data (remove some trailing zeros)#
    mutate(tmp_reverseSign = if_else(M_AVG > 0, 0, 1, missing = NULL),
           tmp_cumSum = cumsum(tmp_reverseSign),
           zeroDate = max(Date[tmp_cumSum < 10])) %>%
    filter(Date <= zeroDate) %>%
    # calculate true cutPoint/endDate date based on newly trimmed data #
    mutate(endDate = Date[tail(breakpoints(M_AVG ~ Date)$breakpoints,1)]) %>%
    filter(Date <= endDate)
}
