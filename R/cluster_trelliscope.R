#' @title cluster_trelliscope
#' @import tidyverse lubridate trelliscopejs ggplot2 rbokeh
#' @export
#' @description **Intented to take data in long format for trelliscopejs use**
#' 
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' * Prediction
#' @param trans Scale of the y-axis, see `ggplot2::scale_y_continuous()`
#' * Default is `identity`
#' @param name Name of the trelliscope view
#' * Default is `Cluster Results`
#' @param group Group of trelliscope views
#' * Default is `common`
#' @param path Base directory of trelliscope app
#' * Default is `~/trelliscope`
#' @param selfContained Create the individual pre-rendered panels
#' * Default is `FALSE`
#' 
#' @return A trelliscopejs object
#' 
#' @seealso 
#' * `ggplot2::scale_y_continuous()`
#' * [`trelliscopejs`](https://hafen.github.io/trelliscopejs/)
#'
#' @examples test
cluster_trelliscope <- function(data, trans = "identity",
                                name = "Cluster Results", group = "common", 
                                path = "~/trelliscope", selfContained = FALSE) {
  
  data %>%
    mutate(Date = ymd(Date)) %>%
    group_by(prediction) %>%
    nest() %>%
    mutate(
      cogs = map_cog(data, ~ data_frame(
        numAccts = length(unique(.$AccountNumber)),
        total = sum(.$Count),
        mean = mean(.$Count),
        sd = sd(.$Count),
        cv = (sd(.$Count) / mean(.$Count)),
        countsPerAcctNum = sum(.$Count) / length(unique(.$AccountNumber))
      )),
      panel = map_plot(data, 
                       ~ ggplot(., aes(x = Date, y = Count, group = AccountNumber)) +
                         geom_line(alpha = .05) +
                         scale_y_continuous(trans = trans) +
                         labs(y = "Normalized Count") +
                         theme_bw()
      )
    ) %>%
    trelliscope(name, group = group, path = path, self_contained = selfContained)
}