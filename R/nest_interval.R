#' @title nest_append_interval
#' @import tidyverse
#' @export
#' @description **Designed to create cognostics by certain interval/ratio in the past**
#'
#' @param nestTib Nested tibble with `AccountNumber`` as identifier
#' @param rawData Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' @param type Must be one of following strings:
#' * `years`
#' * `months`
#' * `weeks`
#' * `days`
#' @param interval Must be positive integer excluding zero
#'
#' @return nested tibble
#'
#' @examples test
nest_append_interval <- function(nestTib, rawData, type, interval) {

  organizedData <- nest_interval(rawData, type, interval)

  nestTib %>%
    left_join(organizedData, by = "AccountNumber")
}

######################################################################

#' @title nest_interval
#' @import tidyverse lubridate
#' @importFrom purrr map2
#' @importFrom Hmisc capitalize
#' @export
#' @description **Designed to create cognostics by certain interval/ratio in the past**
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' @param type Must be one of following strings:
#' * `years`
#' * `months`
#' * `weeks`
#' * `days`
#' @param interval Must be positive integer excluding zero
#'
#' @return nested joined tibble
#'
#' @examples test
nest_interval <- function(data, type, interval) {

  derefType <- "type"
  
  typeCapital <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))
  
  intervalData <- data %>%
    select(AccountNumber, Date, Count) %>%
    group_by(AccountNumber) %>%
    filter(Date %within% ((max(Date) - do.call(get(derefType), list(interval * 2)))
                          %--% max(Date)))

  allowedData <- intervalData %>%
    # Remove all accounts that don't meet ENTIRE interval requirements
    filter(min(Date) == max(Date) - do.call(get(derefType), 
                                            list(interval * 2)))
  
  rightData <- allowedData %>%
    filter(Date >= ((max(Date) - do.call(get(derefType), list(interval))))) %>%
    nest_core_interval(type, interval, "R")
  
  leftData <- allowedData %>%
    filter(Date < ((max(Date) - do.call(get(derefType), list(interval))))) %>%
    nest_core_interval(type, interval, "L")
  
  ratioData <- left_join(leftData, rightData, by = "AccountNumber")
  
  ratioData %<>%
    mutate(
      !!paste0(letter, interval, "_Ratios"):= map2(ratioData[[2]],
                                                   ratioData[[3]],
                                                   ~ as_tibble(.y/.x) %>%
                                                     rename_all(funs(sub('_.', "_Ratio", .))))) %>%
    select(AccountNumber, contains("Ratio"))
  
  intervalData %<>%
    filter(min(Date) == max(Date) - do.call(get(derefType), 
                                            list(interval * 2))) %>%
    nest_core_interval(type, interval, "A")
  
  left_join(intervalData, leftData, by = "AccountNumber") %>%
    left_join(., rightData, by = "AccountNumber") %>%
    left_join(., ratioData, by = "AccountNumber")
}

######################################################################

#' @title nest_core_interval
#' @import tidyverse Hmisc multidplyr
#' @export
#' @description **Designed to create cognostics by certain interval/ratio in the past**
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' @param type Must be one of following strings:
#' * `years`
#' * `months`
#' * `weeks`
#' * `days`
#' @param interval Must be positive integer excluding zero
#' @param divide Must be one of following strings:
#' * `A`
#' * `R`
#' * `L`
#'
#' @return nested tibble
#'
#' @examples test
nest_core_interval <- function(data, type, interval, divide) {

  letter <- capitalize(substr(type, 1, 1))
  
  data %>%
    select(AccountNumber, Date, Count) %>%
    ungroup() %>%
    partition(AccountNumber) %>%
    summarise(!!paste0(letter, interval, "_", divide, "_Count") := sum(Count),
              !!paste0(letter, interval, "_", divide, "_Mean") := mean(Count),
              !!paste0(letter, interval, "_", divide, "_Median") := median(Count),
              !!paste0(letter, interval, "_", divide, "_SD") := sd(Count),
              !!paste0(letter, interval, "_", divide, "_Max") := max(Count),
              !!paste0(letter, interval, "_", divide, "_Min") := min(Count),
              !!paste0(letter, interval, "_", divide, "_CV") := (sd(Count) / mean(Count)),
              
              #######################################################################
              
              #!!paste0(letter, interval, "_", divide, "_SLP") := (lm(Count ~ as.numeric(Date),
              #                                                       data = .)[["coefficients"]][2]),
              !!paste0(letter, interval, "_", divide, "_OOC2") := (sum(Count >= (mean(Count) + (2 * sd(Count))))),
              !!paste0(letter, interval, "_", divide, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count))))),
              
              #######################################################################
              
              !!paste0(letter, interval, "_", divide, "_P") := tsCogs::find_SignedSequence(Count, 1),
              !!paste0(letter, interval, "_", divide, "_N") := tsCogs::find_SignedSequence(Count, -1),
              !!paste0(letter, interval, "_", divide, "_Z") := tsCogs::find_SignedSequence(Count, 0),
              
              #######################################################################
              
              !!paste0(letter, interval, "_", divide, "_I") := tsCogs::find_LadderSequence(Count, "I"),
              !!paste0(letter, interval, "_", divide, "_D") := tsCogs::find_LadderSequence(Count, "D"),
              !!paste0(letter, interval, "_", divide, "_IP") := tsCogs::find_LadderSequence(Count, "IP"),
              !!paste0(letter, interval, "_", divide, "_DP") := tsCogs::find_LadderSequence(Count, "DP"),
              !!paste0(letter, interval, "_", divide, "_IN") := tsCogs::find_LadderSequence(Count, "IN"),
              !!paste0(letter, interval, "_", divide, "_DN") := tsCogs::find_LadderSequence(Count, "DN")
    ) %>%
    collect() %>%
    group_by(AccountNumber) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, interval, "_", divide, "_Cognostics") := Cogs)
}

######################################################################

#' @title nest_interval_unnest
#' @import tidyverse purrr magrittr
#' @export
#' @description **Designed to unnest a nested tibble, which is a list of lists**
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#'
#' @return unnested tibble
#' 
#' @examples test
nest_interval_unnest <- function(data) {
  
  data %>%
    modify_if(is_list, ~ modify_if(., is_null, 
                                   ~ tibble(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))) %>%
    unnest() %>%
    select(-num_range("", 1:1000000))
}
