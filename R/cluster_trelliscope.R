#' cluster_trelliscope
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Date -- Daily consecutive --
#' 3. Count -- Must include all real numbers --
#' Intended to take data from long SQL Server table format
#'
#' @import tidyverse lubridate trelliscope
#' 
#' @return spread tibble
#' @export
#' 
#' @examples test
cluster_trelliscope <- function(data) {
  
  normalize_weekly(data) %>%
    kMeans_sparkly() %>%
    gen_trelliscope()
}