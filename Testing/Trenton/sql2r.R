
library(tsCogs)
library(RODBC) # read SQL tables


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
cutData <- rawData %>% 
  cut_point()
toc()


# apply rules: >600 claims total and >90 days of claims (14 sec) (removed roughly 5,500 accounts)
cutData %<>%
  group_by(AccountNumber) %>%
  mutate(totalCount = sum(Count),
         numDays = n()) %>%
  filter(totalCount >= 600,
         numDays >= 90) %>%
  select(-tmp_reverseSign, -tmp_cumSum)

toc()

# grab only AccountNumber after applying the rules
keepIDs <- cutData %>%
  pull(AccountNumber) %>%
  unique()

# determine cluster size (elbow plot) #
# run once 
# sc <- spark_connect(master = "local")
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
# tic()
# clusterData %>%
#   select(-features) %>%
#   gather("Date", "Count", -AccountNumber, -prediction) %>% #, -Truth) %>%
#   cluster_trelliscope(trans = "log10", 
#                       name = "Cluster Results 100 (log10)", 
#                       group = "eClaims", 
#                       path = "~/trelliscopeDisplays", 
#                       selfContained = F)
# toc()
# 
# 
# # trelliscope the cut point results 
# ### this won't work with cutData as input - need to fix  ###
# tic()
# cutData %>%
#   left_join(truthData, by = "AccountNumber") %>%
#   group_by(AccountNumber) %>%
#   cutPoint_trelliscope(name = "cutPoint Results", 
#                       group = "eClaims", 
#                       path = "~/trelliscopeDisplays", 
#                       selfContained = F)
toc()


# cognostic/feature set generation (24 min)
cogsData <- cutData %>%
  nest_todo() %>%
  nest_append_interval(cutData, "years", 1) %>%
  nest_append_interval(cutData, "months", 6) %>%
  nest_append_interval(cutData, "months", 3) %>%
  nest_append_interval(cutData, "weeks", 6) %>%
  nest_append_interval(cutData, "days", 14) %>%
  left_join(truthData, by = "AccountNumber")
toc()
# load("~/eClaimEnv-wClaimsCogs20180308.rdata")

cogsData$Truth <- factor(cogsData$Truth, levels = c("Healthy", "Dropped"))

# split train/validation datasets #
library(caret)
library(R.utils)

cogsData %>%
  group_by(AccountNumber) %>%
  

bob <- cogsData %>%
  ungroup() %>%
  select(D_Cognostics, W_Cognostics, M_Cognostics, Y_Cognostics, Truth) %>%
  unnest(D_Cognostics, W_Cognostics, M_Cognostics, Y_Cognostics)

# which colnames are all NAs
unlist(lapply(bob, function(X) all(is.na(X))))


bob %<>% 
  select(-D_N, -D_IN, -D_DN, -W_N, -W_IN, -W_DN, -M_N, -M_IN, -M_DN, -Y_N, -Y_IN, -Y_DN)

bob[is.na(bob)] <- 0

validationIndex = createDataPartition(bob$Truth, p = 0.8, list = F) # 80% train 20% validation
validData = bob[-validationIndex,]
trainData = bob[validationIndex,]

# validationIndex = createDataPartition(cogsData$Truth, p = 0.8, list = F) # 80% train 20% validation
# validData = cogsData[-validationIndex,]
# trainData = cogsData[validationIndex,]
 
# can't run it in parallel on a Windows machine
cl = parallel::makeCluster(7) # create 7 node cluster to run in parallel
control = trainControl(method = "repeatedcv", number = 2, repeats = 2, allowParallel = T) # 4 fold cross-validation repeated 10 times
metric = "Kappa" #"Accuracy" usually, but here there is a low percentage of "At Risk" accounts so use Kappa


# these methods worked (others above did not) #
methods2 = c("adaboost", "AdaBoost.M1", "AdaBag", #"ada", 
             "LogitBoost", "rpartScore",
             "rpartCost", "deepboost", "stepLDA", "naive_bayes", "nb", "stepQDA",
             "rFerns", "rocc", "rpart", "rpart1SE", "rpart2")
methods2fast = c("LogitBoost", "rpartScore",
             "rpartCost", "stepLDA", "naive_bayes", "nb", "stepQDA",
             "rocc", "rpart", "rpart1SE", "rpart2")


# packages included: fastAdaBoost, adabag, ada, caTools, rpart, deepboost, klaR, MASS, naivebayes, rFerns, rocc, rpartScore

tic()
# build multiple models #
set.seed(1234)
out2 <- list()
m <- 1
for(i in methods2fast) {
  fit <- tryCatch( train(Truth ~.,
                        data = trainData, #%>% select(-AccountNumber, -prediction),
                        method = i,
                        trControl = control,
                        metric = metric),
                  error = function(e) "bad run" )
  if(fit != "bad run") {
    out2[[m]] <- fit
    names(out2)[m] <- i
    m <- m + 1
  }
  cat(i, "; ")
toc()
}
toc()

results = resamples(out2)

summary(results)
dotplot(results)

##  Model Validation  ##
predictions = list()
pred.mat = list()
for(i in 1:length(out2)) {
  predictions[[i]] = predict(out2[[i]], validData)
  pred.mat[[i]] = confusionMatrix(predictions[[i]], validData$Truth)
}

# overall accuracy #
unlist(lapply(pred.mat, function(X) X$overall[1]))
# confusion matrix #
lapply(pred.mat, function(X) X$table)
# recall #
unlist(lapply(pred.mat, function(X) X$byClass['Recall'])) # not available for 3-class outcomes
# precision #
unlist(lapply(pred.mat, function(X) X$byClass['Precision'])) # not available for 3-class outcomes



