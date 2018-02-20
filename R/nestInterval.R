#' nest_append_interval
#'
#' @param nestTib Nested tibble with "AccountNumber" as identifier
#' @param rawData Must include, at minimun, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#' @param type Must be one of following strings:
#' 1. "years"
#' 2. "months"
#' 3. "weeks"
#' 4. "days"
#' @param interval Must be positive integer excluding zero
#' 
#' @import tidyverse
#'
#' @return nested tibble
#' @export
#'
#' @examples test
nest_append_interval <- function(nestTib, rawData, type, interval) {
  
  library(tidyverse)

  organizedData <- nest_interval(rawData, type, interval)

  nestTib %>%
    left_join(organizedData, by = "AccountNumber")
}

######################################################################

#' nest_interval
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#' @param type Must be one of following strings:
#' 1. "years"
#' 2. "months"
#' 3. "weeks"
#' 4. "days"
#' @param interval Must be positive integer excluding zero
#' 
#' @import tidyverse lubridate
#' @importFrom purrr map2
#' @importFrom Hmisc capitalize
#'
#' @return nested joined tibble
#' @export
#'
#' @examples test
nest_interval <- function(data, type, interval) {
  
  devtools::use_package("tidyverse")
  devtools::use_package("lubridate")
  devtools::use_package("purr")
  devtools::use_package("Hmisc")

  derefType <- "type"

  typeCapital <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))

  allData <- data %>%
    select(AccountNumber, Date, Count) %>%
    filter(AccountNumber != 0,
           Date %within% ((max(Date) - do.call(get(derefType), list(interval * 2)))
                          %--% max(Date))) %>%
    group_by(AccountNumber, tmpColName = cut(Date, 2))

  rightData <- allData %>%
    ungroup() %>%
    filter(Date >= ((max(Date) - do.call(get(derefType), list(interval)))) - 1) %>%
    nest_core_interval(type, interval, "R")

  leftData <- allData %>%
    ungroup() %>%
    filter(Date < ((max(Date) - do.call(get(derefType), list(interval)))) - 1) %>%
    nest_core_interval(type, interval, "L")

  ratioData <- left_join(leftData, rightData, by = "AccountNumber")

  ratioData %<>%
    mutate(
      !!paste0(letter, interval, "_Ratios"):= map2(ratioData[[2]],
                                                   ratioData[[3]],
                                                   ~ as_tibble(.y/.x) %>%
                                                     rename_all(funs(sub('_.', "_Ratio", .))))) %>%
    select(AccountNumber, contains("Ratio"))

  allData %<>%
    ungroup() %>%
    nest_core_interval(type, interval, "A")

  left_join(allData, leftData, by = "AccountNumber") %>%
    left_join(., rightData, by = "AccountNumber") %>%
    left_join(., ratioData, by = "AccountNumber") %>%
    group_by(AccountNumber)
}

######################################################################

#' nest_core_interval
#'
#' @param data Must include, at minimun, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#' @param type Must be one of following strings:
#' 1. "years"
#' 2. "months"
#' 3. "weeks"
#' 4. "days"
#' @param interval Must be positive integer excluding zero
#' @param divide Must be one of following strings:
#' 1. "A"
#' 2. "R"
#' 3. "L"
#' 
#' @import tidyverse Hmisc
#'
#' @return nested tibble
#' @export
#'
#' @examples test
nest_core_interval <- function(data, type, interval, divide) {
  
  devtools::use_package("tidyverse")
  devtools::use_package("Hmisc")

  letter <- capitalize(substr(type, 1, 1))

  data %>%
    select(AccountNumber, Date, Count) %>%
    group_by(AccountNumber) %>%
    summarise(!!paste0(letter, interval, "_", divide, "_Count") := sum(Count),
              !!paste0(letter, interval, "_", divide, "_Mean") := mean(Count),
              !!paste0(letter, interval, "_", divide, "_Median") := median(Count),
              !!paste0(letter, interval, "_", divide, "_SD") := sd(Count),
              !!paste0(letter, interval, "_", divide, "_Max") := max(Count),
              !!paste0(letter, interval, "_", divide, "_Min") := min(Count),
              !!paste0(letter, interval, "_", divide, "_CV") := (sd(Count) / mean(Count)),

              #######################################################################

              !!paste0(letter, interval, "_", divide, "_SLP") := (lm(Count ~ as.numeric(Date),
                                                                     data = .)[["coefficients"]][2]),
              !!paste0(letter, interval, "_", divide, "_OOC2") := (sum(Count >= (mean(Count) + (2 * sd(Count))))),
              !!paste0(letter, interval, "_", divide, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count))))),

              #######################################################################

              !!paste0(letter, interval, "_", divide, "_P") := find_SignedSequence(Count, 1),
              !!paste0(letter, interval, "_", divide, "_N") := find_SignedSequence(Count, -1),
              !!paste0(letter, interval, "_", divide, "_Z") := find_SignedSequence(Count, 0),

              #######################################################################

              !!paste0(letter, interval, "_", divide, "_I") := find_LadderSequence(Count, "I"),
              !!paste0(letter, interval, "_", divide, "_D") := find_LadderSequence(Count, "D"),
              !!paste0(letter, interval, "_", divide, "_IP") := find_LadderSequence(Count, "IP"),
              !!paste0(letter, interval, "_", divide, "_DP") := find_LadderSequence(Count, "DP"),
              !!paste0(letter, interval, "_", divide, "_IN") := find_LadderSequence(Count, "IN"),
              !!paste0(letter, interval, "_", divide, "_DN") := find_LadderSequence(Count, "DN")
    ) %>%
    group_by(AccountNumber) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, interval, "_", divide, "_Cognostics") := Cogs)
}
