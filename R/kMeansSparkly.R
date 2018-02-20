#' kMeans_sparkly
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Many date columns, with weekly counts within cells -- Weekly consecutive --
#' Intended to take data in weekly long format
#'
#' @import tidyverse sparklyr
#' 
#' @return spread tibble
#' @export
#' 
#' @examples test
kMeans_sparkly <- function(data) {
  
  sc <- spark_connect(master = "local") # setup spark connection
  normalData_tbl <- copy_to(sc, data %>% 
                              ungroup(),
                            "rawData", overwrite = TRUE)
  
  mlKmeans <- normalData_tbl %>%
    ml_kmeans(~.-AccountNumber, centers = 200)
  
  ml_predict(mlKmeans, normalData_tbl) %>%
    as_tibble()
}