#' @title cut_point
#' @import tidyverse lubridate
#' @export
#' @description **Designed to implement `cut_point` algorithin on column**
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' @param movAvg Moving average 
#' * Default is `21`
#' @param startQuant Quantile percentage to determine start date cut_point
#' * Default is `0.2` 
#' @param movAvg_zeroDays Number of consecetive moving averages evaluated at zero to begin end date cut_point process
#' * Defaults to `1`
#' @param dialBack Number of days to 'dialBack' to determine a new end date cutPoint
#' * Defaults to `0`
#'
#' @return filtered tibble with `cut_point` algorithim performed
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