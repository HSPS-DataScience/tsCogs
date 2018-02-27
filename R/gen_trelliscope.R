#' gen_trelliscope
#'
#' @param data Must include, at minimum, following columns:
#' 1. AccountNumber -- Unique numeric identifier --
#' 2. Many date columns, with weekly counts within cells -- Weekly consecutive --
#' 3. features
#' 4. prediction
#' Intended to take data in weekly long format for trelliscope use
#' @import tidyverse lubridate trelliscope
#' 
#' @return trelliscope
#' @export
#'
#' @examples test
gen_trelliscope <- function(data) {
  
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
        C_AN = sum(.$Count) / length(unique(.$AccountNumber))
      )),
      panel = map_plot(data, ~ ggplot(., aes(x = Date, y = Count, group = AccountNumber)) +
                         geom_line(alpha = .05) +
                         theme_bw() +
                         labs(x = "Date", y = "Count")
      )
    ) %>%
    trelliscope("Cluster Results", self_contained = T)
}