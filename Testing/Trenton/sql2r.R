
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
  left_join(truthData, by = "AccountNumber") %>%
  group_by(AccountNumber)
  
# add the truth to the cluster data
clusterData %<>% 
  left_join(truthData %>% select(-prediction), by = "AccountNumber")

# trelliscope the cluster results 
# Make sure to gather before you bring it in
tic()
clusterData %>%
  select(-features) %>%
  gather("Date", "Count", -AccountNumber, -prediction, -Truth) %>%
  cluster_trelliscope(trans = "log10", 
                      name = "Cluster Results 100 (log10)", 
                      group = "eClaims", 
                      path = "~/trelliscopeDisplays", 
                      selfContained = F)
toc()


# trelliscope the cut point results 
tic()
cutPtData %>%
  cutPoint_trelliscope(name = "cutPoint Results", 
                      group = "eClaims", 
                      path = "~/trelliscopeDisplays", 
                      selfContained = F)
toc()

bob <- cutPtData %>%
  filter(AccountNumber %in% c("75815", "15300", "76174", "47509"))
# cognostic/feature set generation #
tic()
cogsData <- bob %>%
  nest_todo() %>%
  nest_append_interval(bob, "years", 1) %>%
  nest_append_interval(cutPtData, "months", 6) %>%
  nest_append_interval(cutPtData, "months", 3) %>%
  nest_append_interval(cutPtData, "weeks", 6) %>%
  nest_append_interval(cutPtData, "days", 14)
toc()


