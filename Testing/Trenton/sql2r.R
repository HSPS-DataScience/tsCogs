
library(tsCogs)
library(tidyverse)
library(lubridate) # time
library(RODBC) # read SQL tables
library(cluster)
library(sparklyr)
library(tictoc)
library(Hmisc)
library(pracma)
library(trelliscopejs)
library(magrittr)


tic()
###  Read in Daily Profiles of eClaims submissions created in SQL table by Trenton  ###
sqlFilename <- 'dbo.eClaimDailyProfilesExpanded' 
cn <- odbcDriverConnect("Driver={SQL Server Native Client 11.0};Server=hspsdata.nt.local;Database=SupportReports;Uid=USHSI/trenton.pulsipher;Pwd=22AngelA;trusted_connection=yes;",
                       believeNRows = F)
d <- sqlFetch(cn, sqlFilename) # 4 mins, ~625 Mb sized object, ~40.8M rows for old data
toc()

saveRDS(d, file = "~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll.rds")
# load("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll.rds")

# make minor initial adjustments
rawData <- d %>%
  mutate(AccountNumber = as.character(AccountNumber)) %>%
  as.tibble() %>%
  rename(Date = ymd) %>%
  group_by(AccountNumber) %>%
  filter(!is.na(AccountNumber),
         Date >= ymd("2014-11-01")) %>%
  arrange(Date)

# normalize (shape) data as weekly profiles
normWeekData <- normalize_weekly(rawData) 

# cluster using kmeans
clusterData <- normWeekData %>%
    kMeans_sparkly() 


clusterData %>%
    gen_trelliscope()



