#' @title kMeans_sparkly
#' @import tidyverse sparklyr
#' @export
#' @description **Intended to create kmeans from sparklyr**
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' @param centers Number of kmeans centers
#' * Default is `10`
#' 
#' @return spread tibble
#' @export
#' 
#' @examples test
kMeans_sparkly <- function(data, centers = 10) {
  
  sc <- spark_connect(master = "local") # setup spark connection
  normalData_tbl <- copy_to(sc, data %>% 
                              ungroup(),
                            "rawData", overwrite = TRUE)
  
  mlKmeans <- normalData_tbl %>%
    ml_kmeans(~.-AccountNumber, centers = centers, seed = 1234)
  
  ml_predict(mlKmeans, normalData_tbl) %>%
    as_tibble()
}