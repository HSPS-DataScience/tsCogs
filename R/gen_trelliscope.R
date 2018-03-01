#' gen_trelliscope
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Many date columns, with weekly counts within cells -- Weekly consecutive --
#' 3. features
#' 4. prediction
#' @param log10 scale the y-axis by log base 10, defaults to FALSE
#' @param name name of the trelliscope view, defaults to "Cluster Results"
#' @param group group of trelliscope views -- defaults to "common"
#' @param path the base directory of trelliscope app, defaults to "~/trelliscope"
#' @param selfContained defaults to TRUE
#' Intended to take data in weekly long format for trelliscope use
#' @import tidyverse lubridate trelliscopejs rbokeh
#' 
#' @return trelliscopejs
#' @export
#'
#' @examples test
gen_trelliscope <- function(data, log10 = FALSE, name = "Cluster Results", group = "common", path = "~/trelliscope", selfContained = T) {
  
  data %>%
    select(-features) %>%
    gather("Date", "Count", -AccountNumber, -prediction) %>%
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
                       # ~ ggplot(., aes(x = Date, y = Count, group = AccountNumber)) +
                       #   geom_line(alpha = .05) +
                       #   theme_bw() +
                       #   labs(x = "Date", y = "Count")
                       ~ figure(., ylab = "Normalized Count") %>%
                         ly_lines(x = Date, y = Count, group = AccountNumber, alpha = .05, legend = F) %>%
                         y_axis(log = log10) 
                         
      )
    ) %>%
    trelliscope(name, group = group, path = path, self_contained = selfContained)
}