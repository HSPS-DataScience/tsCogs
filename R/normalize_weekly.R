#' @title normalize_weekly
#' @import tidyverse lubridate
#' @export
#' @description **Designed to normalize at the weekly level**
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' 
#' @return spread tibble
#' 
#' @examples test
normalize_weekly <- function(data) {
  
  data %>%
    # Create week number and summarise by week
    mutate(Week = floor_date(Date, "week")) %>%
    group_by(AccountNumber, Week) %>%
    summarise(Count = sum(Count)) %>% #n()) %>%
    rename(Date = Week) %>%
    arrange(Date) %>%
    # Normalize the profiles
    mutate(meanCount = mean(Count, na.rm = T), 
           normCount = Count / meanCount) %>%
    select(AccountNumber, Date, normCount) %>%
    spread(key = Date, value = normCount) %>%
    replace(is.na(.), 0)
}