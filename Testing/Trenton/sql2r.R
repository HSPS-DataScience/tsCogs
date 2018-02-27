
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
sqlFilename <- 'dbo.eClaimDailyProfilesExpanded' # 3.6 mins, ~555 Mb, ~36.3M rows for new data
cn <- odbcDriverConnect("Driver={SQL Server Native Client 11.0};Server=hspsdata.nt.local;Database=SupportReports;Uid=USHSI/trenton.pulsipher;Pwd=22AngelA;trusted_connection=yes;",
                       believeNRows = F)
d <- sqlFetch(cn, sqlFilename) # 4 mins, ~625 Mb sized object, ~40.8M rows for old data
toc()

saveRDS(d, file = "~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll.rds")
# load("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll.rds")
