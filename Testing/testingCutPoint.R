cut_Points <- function(data, movAvg = 21, startQuant = 0.2, movAvg_zeroDays = 1, dialBack = 35) {
  
  library(tidyverse)
  library(magrittr)
  library(pracma)
  library(lubridate)
  
  data %>%
    filter(Cluster3 == "Dropped") %>%
    group_by(AccountNumber) %>%
    # calculate startDate #
    mutate(M_AVG = movavg(Count, movAvg, "s"),
           quant = quantile(M_AVG[M_AVG > 0], probs = startQuant),
           startDate = min(Date[(M_AVG > quant)])) %>%
    filter(Date >= startDate) %>%
    # calculate temporary endDate of data (remove some trailing zeros)#
    mutate(tmp_reverseSign = if_else(M_AVG > 0, 0, 1, missing = NULL),
           tmp_cumSum = cumsum(tmp_reverseSign),
           zeroDate = max(Date[tmp_cumSum < movAvg_zeroDays]),
           endDate = zeroDate - movAvg,
           cutDate = endDate - dialBack) #%>%
    #filter(Date <= cutDate) #%>%
  # calculate true cutPoint/endDate date based on newly trimmed data #
  #mutate(endDate = Date[tail(breakpoints(M_AVG ~ Date)$breakpoints,1)]) %>%
  #filter(Date <= endDate)
}


accountData <- rawData %>%
  filter(AccountNumber == "8888") %>%
  #filter(Cluster3 == "Dropped") %>%
  cut_Points()

accountData %>%
  ggplot(aes(x = Date, y = M_AVG)) +
  geom_line() +
  geom_point(aes(x = Date, y = Count), data = accountData) +
  geom_vline(aes(xintercept = startDate)) +
  geom_vline(aes(xintercept = cutDate)) +
  geom_vline(aes(xintercept = endDate)) +
  geom_vline(aes(xintercept = zeroDate), color = "blue") +
  theme_bw()
