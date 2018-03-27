#' cut_point_trelliscope
#'
#' @param data Must include, at minimum, following columns:    
#' 1. AccountNumber -- Unique numeric identifier --    
#' 2. Date -- Daily consecutive --    
#' 3. Count -- Must include all real numbers --    
#' Intended to take data from long SQL Server table format    
#' @param movAvg Moving Average -- Defaults to 21    
#' @param trans scale of the y-axis, see ggplot2::scale_x_continuous() -- defaults to "identity" --
#' @param name name of the trelliscope view -- defaults to "Cluster Results" --
#' @param group group of trelliscope views -- defaults to "common" --
#' @param path the base directory of trelliscope app -- defaults to "~/trelliscope" --
#' @param selfContained create the individual pre-rendered panels -- defaults to FALSE --
#'
#' @import tidyverse trelliscopejs pracma magrittr ggplot2 rbokeh
#' 
#' @return trelliscopejs object
#' @export
#' 
#' @examples test
cut_point_trelliscope <- function(data, movAvg = 21, trans = "identity",
                                 name = "cutPoint Results", group = "common", 
                                 path = "~/trelliscope", selfContained = F) {
  
  cutData <- data %>%
    arrange(AccountNumber, Date) %>%
    cut_point() %>%
    slice(1)
  
  data %>%
    arrange(AccountNumber, Date) %>%
    group_by(AccountNumber) %>%
    mutate(M_AVG = movavg(Count, movAvg, "s")) %>%
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
                         geom_point(alpha = 0.5) +
                         geom_line(aes(y = M_AVG)) +
                         geom_vline(aes(xintercept = startDate), color = "blue", linetype = 5) +
                         geom_vline(aes(xintercept = zeroDate), color = "red") +
                         geom_vline(aes(xintercept = endDate), color = "red") +
                         geom_vline(aes(xintercept = cutDate), color = "blue", linetype = 5) +
                         scale_y_continuous(trans = trans) +
                         theme_bw() 
      )
    ) %>%
    trelliscope(name = name, 
                group = group, 
                path = path, 
                self_contained = selfContained)
}