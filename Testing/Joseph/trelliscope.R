library(tsCogs)
library(trelliscopejs)
library(magrittr)


rawData <- readRDS("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll-20180206.rds")

rawData %<>%
  as.tibble() %>%
  rename(Date = ymd) %>%
  group_by(AccountNumber) %>%
  filter(!is.na(AccountNumber)) %>%
  arrange(Date) %>%
  mutate(Week = floor_date(Date, "week")) %>%
  
  group_by(AccountNumber, Week) %>%
  summarise(Count = sum(Count)) %>%
  rename(Date = Week) %>%
  
  mutate(meanCount = mean(Count, na.rm = T), 
         normCount = Count / meanCount) %>%
  select(AccountNumber, Date, normCount) %>%
  spread(key = Date, value = normCount)


sc <- spark_connect(master = "local")
data_tbl <- copy_to(sc, rawDailyProfilesAllNorm[,-1], "data", overwrite = TRUE)
numClusters = c(10,25,50,75,100,150,200,500)
out4 = list()
for(i in 1:length(numClusters)) {
  out4[[i]] <- ml_kmeans(data_tbl, ~., centers = numClusters[i])
  cat(numClusters[i], " ")
}

rawData_tbl <- copy_to(sc, rawData %>%
                         ungroup() %>%
                         select(-AccountNumber), "rawData", overwrite = TRUE)

rawData_tbl %>%
  ml_kmeans(~., centers = 3)


tic()
set.seed(1234)
numClusters = c(10,25,50,100,150,200,300,500,1000)
out = list()
for(i in 1:length(numClusters)) {
  out[[i]] = rawData_tbl %>%
    ml_kmeans(~., centers = numClusters[[i]])
  cat(numClusters[i], " ")
}
toc()

rawData_tbl$`20140928`

bob <- rawData_tbl %>%
  select(`20140928`, `20141005`) %>%
  ml_kmeans(~., centers = 3)

qplot(x = numClusters, y = unlist(lapply(out, function(x) x$cost))) + 
  geom_line() + 
  labs(x = "Number of Clusters", y = "Total W/in Sums of Squares") +
  theme_bw()

