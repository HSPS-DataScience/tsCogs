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


rawData_tbl <- copy_to(sc, rawData %>%
                         ungroup() %>%
                         select(-AccountNumber), "rawData", overwrite = TRUE)

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


qplot(x = numClusters, y = unlist(lapply(out, function(x) x$cost))) + 
  geom_line() + 
  labs(x = "Number of Clusters", y = "Total W/in Sums of Squares") +
  theme_bw()



mlKmeans <- rawData_tbl %>%
  ml_kmeans(~., centers = 200)


predicted <- sdf_predict(mlKmeans, rawData_tbl) #%>%
  collect()
  
  
bob <- as_tibble(predicted)

bob %>%
  select(`20171210`, AccountNumber) %>%
  filter(AccountNumber == "000016")

rawData %>%
  select(`2017-12-10`, AccountNumber) %>%
  filter(AccountNumber == "000016")

jack <- bob %>%
  select(-features) %>%
  filter(prediction == 70) %>%
  gather("Date", "Count", `20140928`:`20180204`) %>%
  mutate(Date = ymd(Date)) %>%
  group_by(prediction)



bob %>%
  select(-features) %>%
  filter(prediction == 70) %>%
  gather("Date", "Count", `20140928`:`20180204`) %>%
  mutate(Date = ymd(Date)) %>%
  group_by(prediction) %>%
  filter(Count > 0) %>%
  nest() %>%
  mutate(
    #cogs = map_cog(data, ~ data_frame(
    #  mean = mean(.$Count)
    #)),
    panel = map_plot(data, ~ ggplot(., aes(x = Date, y = Count, group = AccountNumber)))
                       #geom_line(aes(alpha = .25)))
                       #theme_bw())
  ) %>%
  trelliscope("billybob5", self_contained = T, auto_cog = FALSE)
  








