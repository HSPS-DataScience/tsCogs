#' cutPoint_trelliscope
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#' Intended to take data from long SQL Server table format
#'
#' @import tidyverse trelliscopejs pracma magrittr
#' 
#' @return trelliscopejs object
#' @export
#' 
#' @examples test
cutPoint_trelliscope <- function(data) {
  
  rawData %<>%
    mutate(AccountNumber = as.character(AccountNumber))
  
  cutData <- rawData %>%
    rename(Date = ymd) %>%
    filter(AccountNumber %in% AN) %>%
    arrange(AccountNumber, Date) %>%
    cut_point() %>%
    slice(1)
  
  rawData %>%
    rename(Date = ymd) %>%
    filter(AccountNumber %in% AN) %>%
    arrange(AccountNumber, Date) %>%
    group_by(AccountNumber) %>%
    mutate(M_AVG = movavg(Count, 21, "s")) %>%
    nest() %>%
    mutate(startDate = cutData$startDate,
           zeroDate = cutData$zeroDate,
           endDate = cutData$endDate,
           cutDate = cutData$cutDate) %>%
    unnest() %>%
    group_by(AccountNumber) %>%
    nest() %>%
    mutate(
      panel = map_plot(data, ~ ggplot(., aes(x = Date, y = Count)) +
                         geom_line(aes(y = M_AVG)) +
                         geom_vline(aes(xintercept = startDate), color = "blue") +
                         geom_vline(aes(xintercept = zeroDate), color = "green") +
                         geom_vline(aes(xintercept = endDate), color = "red") +
                         geom_vline(aes(xintercept = cutDate), color = "orange") +
                         theme_bw() +
                         labs(x = "Date", y = "Count")
      )
    ) %>%
    trelliscope("Cut-Point Results", self_contained = F)
}