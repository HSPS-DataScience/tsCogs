#' cut_point
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#' @param movAvg Moving Average -- Defaults to 21
#' @param startQuant Quantile percentage to determine start date cutPoint -- Defaults to 0.2 
#' @param movAvg_zeroDays Number of consecetive moving averages evaluated at zero to begin end date cutPoint process -- Defaults to 1
#' @param dialBack Number of days to 'dialBack' to determine a new end date cutPoint -- Defaults to 0
#'
#' @import tidyverse lubridate
#' 
#' @return filtered tibble
#' @export
#'
#' @examples test
cut_point <- function(data, movAvg = 21, startQuant = 0.2, 
                      movAvg_zeroDays = 1, dialBack = 0) {

  data %>%
    group_by(AccountNumber) %>%
    arrange(Date) %>%
    # calculate startDate #
    mutate(M_AVG = movavg(Count, movAvg, "s"),
           quant = quantile(M_AVG[M_AVG > 0], probs = startQuant),
           startDate = min(Date[(M_AVG > quant)])) %>%
    filter(Date >= startDate) %>%
    # calculate temporary endDate of data (remove some trailing zeros)#
    mutate(tmp_reverseSign = if_else(M_AVG > 0, 0, 1, missing = NULL),
           tmp_cumSum = cumsum(tmp_reverseSign),
           zeroDate = max(Date[tmp_cumSum < movAvg_zeroDays]),
           endDate = zeroDate - movAvg,
           cutDate = endDate - dialBack) %>%
    filter(Date <= cutDate)
}