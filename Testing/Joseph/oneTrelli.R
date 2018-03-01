library(tsCogs)
library(tidyr)
library(rbokeh)

d <- mpg %>%
  group_by(manufacturer, class) %>%
  nest() %>%
  mutate(panel = map_plot(data,
                          ~ figure(xlab = "City mpg", ylab = "Highway mpg") %>%
                            ly_points(cty, hwy, data = .x)))

d %>%
  trelliscope(name = "city_vs_highway_mpg",
              self_contained = FALSE)