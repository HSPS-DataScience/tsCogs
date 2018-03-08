
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
d <- readRDS("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll.rds")

# make minor initial adjustments (3 mins)
tic()
rawData <- d %>%
  as.tbl() %>%
  mutate(AccountNumber = as.character(AccountNumber)) %>%
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
  pull(AccountNumber) %>%
  unique()

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


# add the truth to the cluster data
clusterData %<>% 
  left_join(truthData %>% select(-prediction), by = "AccountNumber")

# trelliscope the cluster results 
# Make sure to gather before you bring it in
tic()
clusterData %>%
  select(-features) %>%
  gather("Date", "Count", -AccountNumber, -prediction) %>% #, -Truth) %>%
  cluster_trelliscope(trans = "log10", 
                      name = "Cluster Results 100 (log10)", 
                      group = "eClaims", 
                      path = "~/trelliscopeDisplays", 
                      selfContained = F)
toc()


# trelliscope the cut point results 
### this won't work with cutData as input - need to fix  ###
tic()
cutData %>%
  left_join(truthData, by = "AccountNumber") %>%
  group_by(AccountNumber) %>%
  cutPoint_trelliscope(name = "cutPoint Results", 
                      group = "eClaims", 
                      path = "~/trelliscopeDisplays", 
                      selfContained = F)
toc()


# bob <- cutData %>%
#   filter(AccountNumber %in% c("0", "75815", "15300", "76174", "47509"))
# cognostic/feature set generation # see limited functions below
tic()
cogsData <- cutData %>%
  select(AccountNumber, Date, Count) %>%
  nest_todo() %>%
  # nest_append_interval(cutData, "years", 1) %>%
  nest_append_interval(cutData, "months", 6) %>%
  # nest_append_interval(cutData, "months", 3) %>%
  nest_append_interval(cutData, "weeks", 6) %>%
  # nest_append_interval(cutData, "days", 14) %>%
  left_join(truthData, by = "AccountNumber") %>%
  group_by(AccountNumber)
toc()

cogsData %>% ungroup() %>% unnest(D_Cognostics, W_Cognostics, M_Cognostics, Y_Cognostics) %>%
  unnest(M6_A_Cognostics, M6_L_Cognostics) %>%
  unnest(M6_R_Cognostics) %>%
  unnest(M6_Ratios) %>%
  unnest(W6_A_Cognostics, W6_L_Cognostics) %>%
  unnest(W6_R_Cognostics) %>%
  unnest(W6_Ratios)



# TESTING #
# create subset of 500 accounts #
# bob <- cutData %>% 
#   filter(AccountNumber %in% sample(keepIDs, 1000))
# # time various combos #
# tic()
# bob %>%
#   select(AccountNumber, Date, Count) %>%
#   nest_todo() %>%
# #  nest_append_interval(bob, "days", 14) %>%
#   nest_append_interval(bob, "weeks", 6) %>%
#   nest_append_interval(bob, "months", 6) #%>% 
# toc()



nest_todo <- function(data) {
  
  day <- nest_core(data, "day")
  week <- nest_core(data, "week")
  month <- nest_core(data, "month")
  year <- nest_core(data, "year")
  
  day %>%
    left_join(., week, by = "AccountNumber") %>%
    left_join(., month, by = "AccountNumber") %>%
    left_join(., year, by = "AccountNumber")
}

nest_core <- function(data, type) {
  
  tmpColName <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))
  
  data %>%
    # filter(AccountNumber != 0) %>%
    select(AccountNumber, Date, Count) %>%
    group_by(AccountNumber, tmpColName = floor_date(Date, type)) %>%
    summarise(Count = sum(Count)) %>%
    group_by(AccountNumber) %>%
    summarise(!!paste0(letter, "_Count") := sum(Count),
              !!paste0(letter, "_Mean") := mean(Count),
              !!paste0(letter, "_Median") := median(Count),
              !!paste0(letter, "_SD") := sd(Count),
              !!paste0(letter, "_Max") := max(Count),
              !!paste0(letter, "_Min") := min(Count),
              !!paste0(letter, "_CV") := (sd(Count) / mean(Count)),
              
              #######################################################################
              
              !!paste0(letter, "_SLP") := (lm(Count ~ as.numeric(tmpColName),
                                              data = .)[["coefficients"]][2]),
              !!paste0(letter, "_OOC2") := (sum(Count >= (mean(Count) + (2 * sd(Count))))),
              !!paste0(letter, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count)))))#,
              
              #######################################################################
              
              # !!paste0(letter, "_P") := find_SignedSequence(Count, 1),
              # !!paste0(letter, "_N") := find_SignedSequence(Count, -1),
              # !!paste0(letter, "_Z") := find_SignedSequence(Count, 0),
              # 
              # #######################################################################
              # 
              # !!paste0(letter, "_I") := find_LadderSequence(Count, "I"),
              # !!paste0(letter, "_D") := find_LadderSequence(Count, "D"),
              # !!paste0(letter, "_IP") := find_LadderSequence(Count, "IP"),
              # !!paste0(letter, "_DP") := find_LadderSequence(Count, "DP"),
              # !!paste0(letter, "_IN") := find_LadderSequence(Count, "IN"),
              # !!paste0(letter, "_DN") := find_LadderSequence(Count, "DN")
    ) %>%
    group_by(AccountNumber) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, "_Cognostics") := Cogs)
}

nest_core_interval <- function(data, type, interval, divide) {
  
  letter <- capitalize(substr(type, 1, 1))
  
  data %>%
    select(AccountNumber, Date, Count) %>%
    group_by(AccountNumber) %>%
    summarise(!!paste0(letter, interval, "_", divide, "_Count") := sum(Count),
              !!paste0(letter, interval, "_", divide, "_Mean") := mean(Count),
              !!paste0(letter, interval, "_", divide, "_Median") := median(Count),
              !!paste0(letter, interval, "_", divide, "_SD") := sd(Count),
              !!paste0(letter, interval, "_", divide, "_Max") := max(Count),
              !!paste0(letter, interval, "_", divide, "_Min") := min(Count),
              !!paste0(letter, interval, "_", divide, "_CV") := (sd(Count) / mean(Count)),
              
              #######################################################################
              
              !!paste0(letter, interval, "_", divide, "_SLP") := (lm(Count ~ as.numeric(Date),
                                                                     data = .)[["coefficients"]][2]),
              !!paste0(letter, interval, "_", divide, "_OOC2") := (sum(Count >= (mean(Count) + (2 * sd(Count))))),
              !!paste0(letter, interval, "_", divide, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count)))))#,
              
              #######################################################################
              
              # !!paste0(letter, interval, "_", divide, "_P") := find_SignedSequence(Count, 1),
              # !!paste0(letter, interval, "_", divide, "_N") := find_SignedSequence(Count, -1),
              # !!paste0(letter, interval, "_", divide, "_Z") := find_SignedSequence(Count, 0),
              # 
              # #######################################################################
              # 
              # !!paste0(letter, interval, "_", divide, "_I") := find_LadderSequence(Count, "I"),
              # !!paste0(letter, interval, "_", divide, "_D") := find_LadderSequence(Count, "D"),
              # !!paste0(letter, interval, "_", divide, "_IP") := find_LadderSequence(Count, "IP"),
              # !!paste0(letter, interval, "_", divide, "_DP") := find_LadderSequence(Count, "DP"),
              # !!paste0(letter, interval, "_", divide, "_IN") := find_LadderSequence(Count, "IN"),
              # !!paste0(letter, interval, "_", divide, "_DN") := find_LadderSequence(Count, "DN")
    ) %>%
    group_by(AccountNumber) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, interval, "_", divide, "_Cognostics") := Cogs)
}
