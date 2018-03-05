#' cluster_trelliscope
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Many date columns, with weekly counts within cells -- Weekly consecutive --
#' 3. features
#' 4. prediction
#' @param trans scale of the y-axis, see ggplot2::scale_x_continuous() -- defaults to "identity" --
#' @param name name of the trelliscope view -- defaults to "Cluster Results" --
#' @param group group of trelliscope views -- defaults to "common" --
#' @param path the base directory of trelliscope app -- defaults to "~/trelliscope" --
#' @param selfContained create the individual pre-rendered panels -- defaults to FALSE --
#' 
#' Intended to take data in weekly long format for trelliscope use
#' @import tidyverse lubridate trelliscopejs ggplot2 rbokeh
#' 
#' @return trelliscopejs
#' @export
#'
#' @examples test
<<<<<<< HEAD
cluster_trelliscope <- function(data, 
                                idCols = c("AccountNumber", "prediction", "Truth"), 
                                trans = "identity", 
                                name = "Cluster Results", 
                                group = "common", 
                                path = "~/trelliscope", 
                                selfContained = F) {
  
  data %>%
    select(-features) %>%
    gather("Date", "Count", -idCols) %>% 
=======
cluster_trelliscope <- function(data, trans = "identity",
                                name = "Cluster Results", group = "common", 
                                path = "~/trelliscope", selfContained = F) {
  
  data %>%
>>>>>>> e194f1d658102065350545f6c5690705c18f25c3
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