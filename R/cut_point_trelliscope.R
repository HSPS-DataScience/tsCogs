#' @title cut_point_trelliscope
#' @import tidyverse trelliscopejs pracma magrittr ggplot2 rbokeh
#' @export
#' @description **Intended to take long data and output trelliscope with `cut_point` vertical lines**
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' @param movAvg Moving average 
#' * Default is `21`
#' @param trans Scale of the y-axis, see `ggplot2::scale_y_continuous()`
#' * Default is `identity`
#' @param name Name of the trelliscope view
#' * Default is `cut_point Results`
#' @param group Group of trelliscope views
#' * Default is `common`
#' @param path Base directory of trelliscope app
#' * Default is `~/trelliscope`
#' @param selfContained Create the individual pre-rendered panels
#' * Default is `FALSE`
#' 
#' @return trelliscopejs object
#' 
#' @examples test
cut_point_trelliscope <- function(data, movAvg = 21, trans = "identity",
                                 name = "cut_point Results", group = "common", 
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
                         geom_point(alpha = 0.3) +
                         geom_line(aes(y = M_AVG), size = 0.7) +
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