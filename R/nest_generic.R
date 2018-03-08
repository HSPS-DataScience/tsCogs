#' nest_todo
#'
#' Designed to create then join four nested tibbles into one tibble
#' with "AccountNumber" being the identifier:
#' 1. Daily
#' 2. Weekly
#' 3. Monthly
#' 4. Yearly
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#' 
#' @import tidyverse
#'
#' @return nested joined tibble
#' @export
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

#' nest_core
#'
#' Designed to append on numerous descriptive numeric cognostics as columns
#' then nest into a tibble with "AccountNumber" being the identifier.
#' Based on `type` parameter, will aggregate and create cognostics by:
#' 1. Day
#' 2. Week
#' 3. Month
#' 4. Year
#'
#' @param data Must include, at minimun, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#' @param type All meaningfull specifications in
#' English language are supported.
#' Stable arguments are:
#' 1. day
#' 2. week
#' 3. month
#' 4. year
#' 
#' @import tidyverse lubridate 
#' @importFrom Hmisc capitalize
#'
#' @return nested tibble
#' @export
#'
#' @examples test
nest_core <- function(data, type) {
  
  tmpColName <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))

  data %>%
   # filter(AccountNumber != 0) %>%
    select(AccountNumber, Date, Count) %>%
    group_by(AccountNumber, tmpColName = floor_date(Date, type)) %>%
    summarise(Count = sum(Count)) %>%
    group_by(AccountNumber) %>%
    summarise(!!paste0(letter, "_Count") := sum(Count),
              !!paste0(letter, "_Mean") := mean(Count),
              !!paste0(letter, "_Median") := median(Count),
              !!paste0(letter, "_SD") := sd(Count),
              !!paste0(letter, "_Max") := max(Count),
              !!paste0(letter, "_Min") := min(Count),
              !!paste0(letter, "_CV") := (sd(Count) / mean(Count)),

              #######################################################################

              !!paste0(letter, "_SLP") := (lm(Count ~ as.numeric(tmpColName),
                                              data = .)[["coefficients"]][2]),
              !!paste0(letter, "_OOC2") := (sum(Count >= (mean(Count) + (2 * sd(Count))))),
              !!paste0(letter, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count))))),

              #######################################################################

              !!paste0(letter, "_P") := find_SignedSequence(Count, 1),
              !!paste0(letter, "_N") := find_SignedSequence(Count, -1),
              !!paste0(letter, "_Z") := find_SignedSequence(Count, 0),

              #######################################################################

              !!paste0(letter, "_I") := find_LadderSequence(Count, "I"),
              !!paste0(letter, "_D") := find_LadderSequence(Count, "D"),
              !!paste0(letter, "_IP") := find_LadderSequence(Count, "IP"),
              !!paste0(letter, "_DP") := find_LadderSequence(Count, "DP"),
              !!paste0(letter, "_IN") := find_LadderSequence(Count, "IN"),
              !!paste0(letter, "_DN") := find_LadderSequence(Count, "DN")
    ) %>%
    group_by(AccountNumber) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, "_Cognostics") := Cogs)
}
