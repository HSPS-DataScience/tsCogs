library(tsCogs)
library(trelliscopejs)
library(magrittr)
library(sparklyr)

# More generic location
rawData <- readRDS("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll-20180206.rds")

# tic()
# set.seed(1234)
# numClusters = c(10,25,50,100,150,200,300,500,1000)
# out = list()
# for(i in 1:length(numClusters)) {
#   out[[i]] = rawData_tbl %>%
#     ml_kmeans(~.-AccountNumber, centers = numClusters[[i]])
#   cat(numClusters[i], " ")
# }
# toc()
# 
# 
# qplot(x = numClusters, y = unlist(lapply(out, function(x) x$cost))) + 
#   geom_line() + 
#   labs(x = "Number of Clusters", y = "Total W/in Sums of Squares") +
#   theme_bw()


######################################
# normalized_Data
######################################

normalizedData <- rawData %>%
  as.tibble() %>%
  mutate(AccountNumber = as.character(AccountNumber)) %>%
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

sc <- spark_connect(master = "local") # setup spark connection
normalData_tbl <- copy_to(sc, normalizedData %>% 
                      ungroup(),
                    "rawData", overwrite = TRUE)

mlKmeans <- normalData_tbl %>%
  ml_kmeans(~.-AccountNumber, centers = 200)

predict <- ml_predict(mlKmeans, normalData_tbl) %>%
  as_tibble()

predict %>%
  select(-features) %>%
  gather("Date", "Count", `2014-09-28`:`2018-02-04`) %>%
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
                       labs(x = "", y = "UN_normalized count")
    )
  ) %>%
  trelliscope("Cluster Results", self_contained = T) 



######################################################################################
                            #UN_normalized_Data
######################################################################################


UN_normalizedData <- rawData %>%
  as.tibble() %>%
  mutate(AccountNumber = as.character(AccountNumber)) %>%
  rename(Date = ymd) %>%
  group_by(AccountNumber) %>%
  filter(!is.na(AccountNumber)) %>%
  arrange(Date) %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(AccountNumber, Week) %>%
  summarise(Count = sum(Count)) %>%
  rename(Date = Week) %>%
  select(AccountNumber, Date, Count) %>%
  spread(key = Date, value = Count)

sc <- spark_connect(master = "local") # setup spark connection
UN_normalData_tbl <- copy_to(sc, UN_normalizedData %>% 
                      ungroup(),
                    "rawData", overwrite = TRUE)

mlKmeans <- UN_normalData_tbl %>%
  ml_kmeans(~.-AccountNumber, centers = 200)

predict <- ml_predict(mlKmeans, UN_normalData_tbl) %>%
  as_tibble()

UN_normalizedData %<>% left_join(predict %>%
                                   select(AccountNumber, prediction),
                                 by = "AccountNumber")

UN_normalizedData %>%
  #select(-features) %>%
  gather("Date", "Count", `2014-09-28`:`2018-02-04`) %>%
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
                       scale_y_log10() +
                       labs(x = "", y = "UN_normalized count")
    )
  ) %>%
  trelliscope("Cluster Results", self_contained = T)




######################################################################################
                                    #Cluster of Clusters
######################################################################################

sc <- spark_connect(master = "local") # setup spark connection
centersData <- copy_to(sc, mlKmeans$centers, "centersData", overwrite = TRUE)

tic()
numClusters = c(3,5,7,10,25,20,50,100,200)
out = list()
for(i in 1:length(numClusters)) {
  out[[i]] = centersData %>%
    #select(centers) %>%
    ml_kmeans(~., centers = numClusters[[i]], seed = 1234)
  cat(numClusters[i], " ")
}
toc()


qplot(x = numClusters, y = unlist(lapply(out, function(x) x$cost))) +
  geom_line() +
  labs(x = "Number of Clusters", y = "Total W/in Sums of Squares") +
  theme_bw()



######################################################################################
                                # Classifying
######################################################################################

normalData_tbl <- copy_to(sc, normalizedData %>% 
                            ungroup(),
                          "rawData", overwrite = TRUE)

mlKmeans <- normalData_tbl %>%
  ml_kmeans(~.-AccountNumber, centers = 200)

predict <- ml_predict(mlKmeans, normalData_tbl) %>%
  as_tibble()

predict %>%
  select(-features) %>%
  gather("Date", "Count", `2014-09-28`:`2018-02-04`) %>%
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
                       labs(x = "", y = "UN_normalized count")
    )
  ) %>%
  trelliscope("Cluster Results", self_contained = T) 

# Drop
163, 160, 152, 155, 11, 65, 104, 150, 59, 124, 73, 125, 154, 28,
88,50,157,188,153

# 2018 Lacks
112,129,175

# middle?
143

#drop?
159

# drop but reverse middle
73,125,88,50,157

#90 day
6,15,28,188,127,157

