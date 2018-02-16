

library(tidyverse)
library(lubridate) # time
library(RODBC) # read SQL tables
library(cluster)
library(biganalytics)
library(sparklyr)
library(dtwclust)
library(tictoc)


tic()
###  Read in Daily Profiles of eClaims submissions created in SQL table by Trenton  ###
sqlFilename <- 'dbo.eClaimDailyProfilesExpanded'
cn <- odbcDriverConnect("Driver={SQL Server Native Client 11.0};Server=hspsdata.nt.local;Database=SupportReports;Uid=USHSI/trenton.pulsipher;Pwd=22AngelA;trusted_connection=yes;",
                       believeNRows = F)
d <- sqlFetch(cn, sqlFilename) # 4 mins, ~610 Mb sized object, ~39.8M rows
toc()

# saveRDS(d, file = "~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll-20180206.rds")
load("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll-20180206.rds")

rawDailyProfilesAllNorm <- d %>%
  as.tibble() %>%
  rename(Date = ymd) %>%
  group_by(AccountNumber) %>%
  filter(!is.na(AccountNumber)) %>%
  arrange(Date) %>%
  # Create week number and summarise by week
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(AccountNumber, Week) %>%
  summarise(Count = sum(Count)) %>% #n()) %>%
  rename(Date = Week) %>%
  # normalize the profiles
  mutate(meanCount = mean(Count, na.rm = T), 
         normCount = Count / meanCount) %>%
  select(AccountNumber, Date, normCount) %>%
  spread(key = Date, value = normCount)


####################################################
# Methods: cluster::kmeans(), cluster::clara(), biganalytics?::bigkmeans(), sparklyr::ml_kmeans(), dtwclust::tsclust()
# numAccts = 100, 1000, 10000
# k = 5, 10, 25


timeClustering <- function(data, numRows, method, k) {
  tic()
    set.seed(1234)
    sampledData <- data %>%
      ungroup() %>%
      sample_n(size = numRows) %>%
      select(-AccountNumber)
  
    if(method == "kmeans") {
      kmeans(sampledData, 
             k, 
             iter.max = 100, 
             nstart = 10, 
             algorithm = "MacQueen")
  
    } else if(method == "clara") {
      clara(sampledData, 
            k = k, 
            samples = 50, 
            medoids.x = F, 
            keep.data = F, 
            rngR = T)
  
    } else if(method == "bigkmeans") {
      bigkmeans(as.matrix(as.data.frame(sampledData)), 
                k, 
                iter.max = 100, 
                nstart = 10) # uses MacQueen
      
    } else if(method == "sparklyr") {
      sc <- spark_connect(master = "local")
      sampledData_tbl <- copy_to(sc, sampledData, "sampledData", overwrite = TRUE)
      sampledData_tbl %>%
        ml_kmeans(~., centers = k)
      
    } else if(method == "dtwClust") {
      tsclust(sampledData, 
              type = "partitional", 
              k = k, 
              seed = 1234, 
              trace = T)
    }
  toc()    
}


timeClustering(rawDailyProfilesAllNorm, 1000, "kmeans", 5)

numRows <- c(100, 1000, 10000)
methods <- c("kmeans", "bigkmeans", "clara", "sparklyr", "dtwClust")
numClusters <- c(5, 25, 50)

tracked <- data.frame(numRows = rep(rep(numRows,3),5),
                      methods = rep(methods, each = 3*3),
                      numClusters = rep(rep(numClusters, each = 3), 5),
                      time = c(0.04, 0.33, 3.78, 0.13, 1.01, 17.97, 0.1, 1.86, 57.45,
                               0.46, 0.51, 2.77, 0.41, 1.10, 20.36, 0.79, 1.92, 62.99,
                               0.03, 0.10, 1.20, 0.17, 0.46, 10.09, 0.03, 1.19, 15.48,
                               18.63, 3.83, 15.59, 2.56, 3.52, 11.03, 2.62, 3.36, 11.64,
                               0.61, 41.05, rep(NA, 7)))

tracked %>%
   ggplot(aes(x = numRows, y = time, colour = methods)) +
   geom_point() +
   geom_line() +
   facet_grid(numClusters~., scales = "free_y") +
   scale_x_log10() +
   theme_bw()             
          

# kmeans
timeClustering(rawDailyProfilesAllNorm, 100, "kmeans", 5) #0.04
timeClustering(rawDailyProfilesAllNorm, 1000, "kmeans", 5) #0.33
timeClustering(rawDailyProfilesAllNorm, 10000, "kmeans", 5) #3.78
timeClustering(rawDailyProfilesAllNorm, 100, "kmeans", 25) #0.13
timeClustering(rawDailyProfilesAllNorm, 1000, "kmeans", 25) #1.01
timeClustering(rawDailyProfilesAllNorm, 10000, "kmeans", 25) #17.97
timeClustering(rawDailyProfilesAllNorm, 100, "kmeans", 50) #0.1
timeClustering(rawDailyProfilesAllNorm, 1000, "kmeans", 50) #1.86
timeClustering(rawDailyProfilesAllNorm, 10000, "kmeans", 50) #57.45
# bigkmeans
timeClustering(rawDailyProfilesAllNorm, 100, "bigkmeans", 5) #0.46
timeClustering(rawDailyProfilesAllNorm, 1000, "bigkmeans", 5) #0.51
timeClustering(rawDailyProfilesAllNorm, 10000, "bigkmeans", 5) #2.77
timeClustering(rawDailyProfilesAllNorm, 100, "bigkmeans", 25) #0.41
timeClustering(rawDailyProfilesAllNorm, 1000, "bigkmeans", 25) #1.10
timeClustering(rawDailyProfilesAllNorm, 10000, "bigkmeans", 25) #20.36
timeClustering(rawDailyProfilesAllNorm, 100, "bigkmeans", 50) #0.79
timeClustering(rawDailyProfilesAllNorm, 1000, "bigkmeans", 50) #1.92
timeClustering(rawDailyProfilesAllNorm, 10000, "bigkmeans", 50) #62.99
# clara
timeClustering(rawDailyProfilesAllNorm, 100, "clara", 5) #0.03
timeClustering(rawDailyProfilesAllNorm, 1000, "clara", 5) #0.10
timeClustering(rawDailyProfilesAllNorm, 10000, "clara", 5) #1.20
timeClustering(rawDailyProfilesAllNorm, 100, "clara", 25) #0.17
timeClustering(rawDailyProfilesAllNorm, 1000, "clara", 25) #0.46
timeClustering(rawDailyProfilesAllNorm, 10000, "clara", 25) #10.09
timeClustering(rawDailyProfilesAllNorm, 100, "clara", 50) #0.03
timeClustering(rawDailyProfilesAllNorm, 1000, "clara", 50) #1.19
timeClustering(rawDailyProfilesAllNorm, 10000, "clara", 50) #15.48
# sparklyr
timeClustering(rawDailyProfilesAllNorm, 100, "sparklyr", 5) #18.63
timeClustering(rawDailyProfilesAllNorm, 1000, "sparklyr", 5) #3.83
timeClustering(rawDailyProfilesAllNorm, 10000, "sparklyr", 5) #15.59
timeClustering(rawDailyProfilesAllNorm, 100, "sparklyr", 25) #2.56
timeClustering(rawDailyProfilesAllNorm, 1000, "sparklyr", 25) #3.52
timeClustering(rawDailyProfilesAllNorm, 10000, "sparklyr", 25) #11.03
timeClustering(rawDailyProfilesAllNorm, 100, "sparklyr", 50) #2.62
timeClustering(rawDailyProfilesAllNorm, 1000, "sparklyr", 50) #3.36
timeClustering(rawDailyProfilesAllNorm, 10000, "sparklyr", 50) #11.64
# dtwClust
timeClustering(rawDailyProfilesAllNorm, 100, "dtwClust", 5) #0.61
timeClustering(rawDailyProfilesAllNorm, 1000, "dtwClust", 5) #0.51
timeClustering(rawDailyProfilesAllNorm, 10000, "dtwClust", 5) #2.77
timeClustering(rawDailyProfilesAllNorm, 100, "dtwClust", 25) #0.41
timeClustering(rawDailyProfilesAllNorm, 1000, "dtwClust", 25) #1.10
timeClustering(rawDailyProfilesAllNorm, 10000, "dtwClust", 25) #20.36
timeClustering(rawDailyProfilesAllNorm, 100, "dtwClust", 50) #0.79
timeClustering(rawDailyProfilesAllNorm, 1000, "dtwClust", 50) #1.92
timeClustering(rawDailyProfilesAllNorm, 10000, "dtwClust", 50) #62.99

# m <- 1
# for(i in numRows) {
#   for(j in methods) {
#     for(k in numClusters) {
#       cat(m, i, j, k)
#       timeClustering(rawDailyProfilesAllNorm, i, j, k)
#       m <- m + 1
#     }
#   }
# }


####################################################


##  Kmeans clustering  ##
set.seed(1234)
numClusters = c(10,25,50,75,100,150,200,500)
out = list()
for(i in 1:length(numClusters)) {
  out[[i]] = kmeans(as.matrix(rawDailyProfilesAllNorm[,-1]), numClusters[i], iter.max = 100, nstart = 10, algorithm = "MacQueen")
  cat(numClusters[i], " ")
}

qplot(x = numClusters, y = unlist(lapply(out, function(x) x$tot.withinss))) + 
  geom_line() + 
  labs(x = "Number of Clusters", y = "Total W/in Sums of Squares") +
  theme_bw()


##  Clara clustering  ##
tic()
set.seed(1234)
numClusters = c(10,25,50,75,100,150,200,500)
out2 = list()
for(i in 1:length(numClusters)) {
  out2[[i]] = clara(rawDailyProfilesAllNorm[,-1], k = numClusters[i], samples = 50, medoids.x = F, keep.data = F, rngR = T)
  cat(numClusters[i], " ")
}
toc()
# probably need to adjust this plot #
# qplot(x = numClusters, y = unlist(lapply(out, function(x) x$tot.withinss))) + 
#   geom_line() + 
#   labs(x = "Number of Clusters", y = "Total W/in Sums of Squares") +
#   theme_bw()


##  Sparklyr kmeans clustering  ##
tic()
sc <- spark_connect(master = "local")
data_tbl <- copy_to(sc, rawDailyProfilesAllNorm[,-1], "data", overwrite = TRUE)
numClusters = c(10,25,50,100,150,200,300,500,1000)
out4 = list()
for(i in 1:length(numClusters)) {
  out4[[i]] <- ml_kmeans(data_tbl, ~., centers = numClusters[i])
  cat(numClusters[i], " ")
}
toc() # 361 sec


##  Time Series clustering  ##
tic()
numClusters = c(2:10,15,25,50)
out3 = list()
for(i in 1:length(numClusters)) {
  out3[[i]] <- tsclust(rawDailyProfilesAllNorm[,-1], type = "partitional", k = numClusters[i], seed = 1234)
  cat(numClusters[i], " ")
}
toc() #
