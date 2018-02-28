#' normalize_weekly
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#' Intended to take data from long SQL Server table format
#'
#' @import tidyverse lubridate
#' 
#' @return spread tibble
#' @export
#' 
#' @examples test
normalize_weekly <- function(data) {
  
  data %>%
    # as.tibble() %>%
    # rename(Date = ymd) %>%
    # group_by(AccountNumber) %>%
    # filter(!is.na(AccountNumber)) %>%
    # arrange(Date) %>%
    # Create week number and summarise by week
    mutate(Week = floor_date(Date, "week")) %>%
    group_by(AccountNumber, Week) %>%
    summarise(Count = sum(Count)) %>% #n()) %>%
    rename(Date = Week) %>%
    arrange(Date) %>%
    # normalize the profiles
    mutate(meanCount = mean(Count, na.rm = T), 
           normCount = Count / meanCount) %>%
    select(AccountNumber, Date, normCount) %>%
    spread(key = Date, value = normCount) %>%
    replace(is.na(.), 0)
}