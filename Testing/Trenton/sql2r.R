
library(tsCogs)
library(RODBC) # read SQL tables
# library(cluster)
# library(sparklyr)
# library(tictoc)
# library(Hmisc)
# library(pracma)
# library(trelliscopejs)
# library(magrittr)


tic()
###  Read in Daily Profiles of eClaims submissions created in SQL table by Trenton  ###
# sqlFilename <- 'dbo.eClaimDailyProfilesExpanded' 
# cn <- odbcDriverConnect("Driver={SQL Server Native Client 11.0};Server=hspsdata.nt.local;Database=SupportReports;Uid=USHSI/trenton.pulsipher;Pwd=22AngelA;trusted_connection=yes;",
#                        believeNRows = F)
# d <- sqlFetch(cn, sqlFilename) # 4 mins, ~625 Mb sized object, ~40.8M rows for old data
# toc()
# 
# saveRDS(d, file = "~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll.rds")
load("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll.rds")

# make minor initial adjustments
tic()
rawData <- d %>%
  mutate(AccountNumber = as.character(AccountNumber)) %>%
  as.tibble() %>%
  rename(Date = ymd) %>%
  group_by(AccountNumber) %>%
  filter(!is.na(AccountNumber),
         Date >= ymd("2014-11-01"),
         Date <= ymd("2018-02-24")) %>%
  arrange(Date)
toc()

# run cutPoint algorithm (5 mins)
tic()
cutData <- rawData %>% 
  cut_point()
toc()


# apply rules: >600 claims total and >90 days of claims (14 sec) (removed roughly 5,500 accounts)
tic()
cutData %<>%
  group_by(AccountNumber) %>%
  mutate(totalCount = sum(Count),
         numDays = n()) %>%
  filter(totalCount >= 600,
         numDays >= 90)
toc()

# grab only AccountNumber after applying the rules
keepIDs <- cutData %>%
  pull(AccountNumber)

# determine cluster size (elbow plot) #
# run once 
# config <- spark_config()
# config$`sparklyr.shell.driver-memory` <- "1G"
# config$`sparklyr.shell.executor-memory` <- "1G"
# config$`spark.yarn.executor.memoryOverhead` <- "512"
# sc <- spark_connect(master = "local", config = config)
# normalData_tbl <- copy_to(sc, 
#                     rawData %>%
#                       ungroup() %>%
#                       filter(AccountNumber %in% keepIDs) %>%
#                       normalize_weekly(), 
#                     "normalData", 
#                     overwrite = TRUE)
# numClusters = c(5,10,20,30,40,50,75,100)
# out = list()
# for(i in 1:length(numClusters)) {
#   mlkModel <- ml_kmeans(normalData_tbl, ~., centers = numClusters[i], seed = 1234)
#   out[[i]] <- ml_compute_cost(mlkModel, normalData_tbl)
#   rm(mlkModel)
#   gc()
#   cat(numClusters[i], " ")
# }
# 
# qplot(x = numClusters[1:length(out)], y = unlist(out)) + 
#   geom_line() + 
#   labs(x = "Number of Clusters", y = "Total W/in Sums of Squares") +
#   theme_bw()



# normalize (shape) data as weekly profiles and cluster using kmeans (2 mins)
tic()
clusterData <- rawData %>%
  ungroup() %>%
  filter(AccountNumber %in% keepIDs) %>%
  normalize_weekly() %>%
  kMeans_sparkly(centers = 100) # seed is set inside the function
toc()

##########
# assign clusterID and Truth back to cutData
truthData <- clusterData %>%
  select(AccountNumber, prediction) %>%
  mutate(Truth = if_else(prediction %in% c(1:5,7,8,14,15,17,18,20,21,26,28,29,
                                           31,32,34,35,37,38,40,45,49,51,54,56,57,59,
                                           60,62:65,57:70,72,75:77,80,84,85,87,
                                           94,98,99), "Healthy", "Dropped")) %>%
  select(AccountNumber, prediction, Truth) %>%
  group_by(AccountNumber) %>%
  slice(1)

cutPtData <- cutData %>%
  left_join(truthData)

###  stopped here  ###





# trelliscope the cluster results 
tic()
clusterData %>%
   gen_trelliscope(trans = "log10", 
                   name = "Cluster Results 100", 
                   group = "eClaims", 
                   path = "~/trelliscopeDisplays", 
                   selfContained = F)
toc()














clusterData %>%
  filter(prediction == 97) %>%
  select(-features) %>%
  gather("Date", "Count", -AccountNumber, -prediction) %>%
  mutate(Date = ymd(Date)) %>%
  group_by(prediction) %>%
  ggplot(aes(x = Date, y = Count, color = AccountNumber)) +
  geom_line(show.legend = FALSE) +
  scale_y_continuous(trans = "identity") +
  facet_wrap(~AccountNumber, scales = "free_y") +
  theme_bw()

rawData %>%
  filter(AccountNumber == 83600) %>%
  cutPoint_trelliscope()


cutData <- rawData %>%
  filter(AccountNumber == 83600) %>%
  arrange(AccountNumber, Date) %>%
  cut_point() %>%
  slice(1)

rawData %>%
  filter(AccountNumber == 83600) %>%
  arrange(AccountNumber, Date) %>%
  group_by(AccountNumber) %>%
  mutate(M_AVG = movavg(Count, 21, "s")) %>%
  nest() %>%
  mutate(startDate = cutData$startDate,
         zeroDate = cutData$zeroDate,
         endDate = cutData$endDate,
         cutDate = cutData$cutDate) %>%
  unnest() %>%
  group_by(AccountNumber) %>%
  nest() %>%
  mutate(
    panel = map_plot(data, ~ ggplot(., aes(x = Date, y = Count)) +
                       geom_line(aes(y = Count, alpha = 0.5)) +
                       geom_line(aes(y = M_AVG)) +
                       geom_vline(aes(xintercept = startDate), color = "blue", linetype = 3) +
                       geom_vline(aes(xintercept = zeroDate), color = "green") +
                       geom_vline(aes(xintercept = endDate), color = "red") +
                       geom_vline(aes(xintercept = cutDate), color = "orange", linetype = 3) +
                       theme_bw() +
                       labs(x = "Date", y = "Count")
    )
  ) %>%
  trelliscope("Cut-Point Results", self_contained = F)



