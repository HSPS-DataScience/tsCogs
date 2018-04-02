#' @title nest_todo
#' @import tidyverse
#' @export
#' @description **Designed to create then join four nested tibbles into one tibble
#' with `AccountNumber` being the identifier by:**
#' * Daily
#' * Weekly
#' * Monthly
#' * Yearly
#' 
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#'
#' @return nested and joined tibble
#'
#' @examples test
nest_todo <- function(data) {
  
  day <- nest_core(data, "day")
  week <- nest_core(data, "week")
  month <- nest_core(data, "month")
  year <- nest_core(data, "year")

  day %>%
    left_join(., week, by = "AccountNumber") %>%
    left_join(., month, by = "AccountNumber") %>%
    left_join(., year, by = "AccountNumber")
}

######################################################################

#' @title nest_core
#' @import tidyverse lubridate multidplyr
#' @importFrom Hmisc capitalize
#' @export
#' @description **Designed to append on numerous descriptive numeric cognostics as columns
#' then nest into a tibble with "AccountNumber" being the identifier.
#' Based on `type` parameter, will aggregate and create cognostics by:**
#' * Day
#' * Week
#' * Month
#' * Year
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' @param type All meaningfull specifications in
#' English language are supported.
#' Stable arguments are:
#' * `day`
#' * `week`
#' * `month`
#' * `year`
#'
#' @return nested tibble
#' 
#' @examples test
nest_core <- function(data, type) {
  
  tmpColName <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))

  data %>%
    select(AccountNumber, Date, Count) %>%
    group_by(AccountNumber, tmpColName = floor_date(Date, type)) %>%
    summarise(Count = sum(Count)) %>%
    partition(AccountNumber) %>%
    summarise(!!paste0(letter, "_Count") := sum(Count),
              !!paste0(letter, "_Mean") := mean(Count),
              !!paste0(letter, "_Median") := median(Count),
              !!paste0(letter, "_SD") := sd(Count),
              !!paste0(letter, "_Max") := max(Count),
              !!paste0(letter, "_Min") := min(Count),
              !!paste0(letter, "_CV") := (sd(Count) / mean(Count)),

              #######################################################################

              # !!paste0(letter, "_SLP") := (lm(Count ~ as.numeric(tmpColName),
              #                                 data = .)[["coefficients"]][2]),
              !!paste0(letter, "_OOC2") := (sum(Count >= (mean(Count) + (2 * sd(Count))))),
              !!paste0(letter, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count))))),

              #######################################################################

              !!paste0(letter, "_P") := tsCogs::find_SignedSequence(Count, 1),
              !!paste0(letter, "_N") := tsCogs::find_SignedSequence(Count, -1),
              !!paste0(letter, "_Z") := tsCogs::find_SignedSequence(Count, 0),

              #######################################################################

              !!paste0(letter, "_I") := tsCogs::find_LadderSequence(Count, "I"),
              !!paste0(letter, "_D") := tsCogs::find_LadderSequence(Count, "D"),
              !!paste0(letter, "_IP") := tsCogs::find_LadderSequence(Count, "IP"),
              !!paste0(letter, "_DP") := tsCogs::find_LadderSequence(Count, "DP"),
              !!paste0(letter, "_IN") := tsCogs::find_LadderSequence(Count, "IN"),
              !!paste0(letter, "_DN") := tsCogs::find_LadderSequence(Count, "DN")
    ) %>%
    collect() %>%
    group_by(AccountNumber) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, "_Cognostics") := Cogs)
}
