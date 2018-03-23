library(tsCogs)
library(RODBC) # read SQL tables


tic()
###  Read in Daily Profiles of eClaims submissions created in SQL table by Trenton  ###
sqlFilename <- 'dbo.eClaimDailyProfilesExpanded'
cn <- odbcDriverConnect("Driver={SQL Server Native Client 11.0};Server=hspsdata.nt.local;Database=SupportReports;Uid=USHSI/trenton.pulsipher;Pwd=22AngelA;trusted_connection=yes;",
                       believeNRows = F)
d <- sqlFetch(cn, sqlFilename) # 4 mins, ~625 Mb sized object, ~40.8M rows for old data
# rawData <- readRDS("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll.rds")
toc()

# make minor initial adjustments (3 mins)
rawData <- d %>%
  as.tbl() %>%
  mutate(AccountNumber = as.character(AccountNumber)) %>%
  rename(Date = ymd) %>%
  group_by(AccountNumber) %>%
  filter(!is.na(AccountNumber),
         Date >= ymd("2014-11-01")) %>% #,
        # Date <= ymd("2018-02-24")) %>%
  arrange(Date)
toc()


# apply rules: >600 claims total and >90 days of claims (14 sec) (removed roughly 5,500 accounts)
ruleData <- rawData %>%
  group_by(AccountNumber) %>%
  mutate(totalCount = sum(Count),
         numDays = n(),
         last2weeks = (max(Date) - 14)) %>%
  mutate(totalCountLast2weeks = sum(Count[Date >= last2weeks])) %>%
  mutate(Outcome = if_else(totalCount < 600, "Small Practice",
                           if_else(numDays < 90, "Still Implementing",
                                   if_else(totalCountLast2weeks > 0, "need to predict", "Gone")))) %>%
  select(AccountNumber, Date, Count, Outcome)

# table summarizing the number of Accounts per Outcome
ruleData %>%
  slice(1) %>%
  ungroup() %>%
  select(Outcome) %>%
  table()

# cognostic/feature set generation (24 min)
# tic()
cogsData <- ruleData %>%
  #  filter(AccountNumber %in% c('001307', '001354', '8888', '000019')) %>%
  filter(Outcome == "need to predict") %>%  
  group_by(AccountNumber) %>%
  nest_todo() %>%
  nest_append_interval(ruleData, "years", 1) %>%
  nest_append_interval(ruleData, "months", 6) %>%
  nest_append_interval(ruleData, "months", 3) %>%
  nest_append_interval(ruleData, "weeks", 6) %>%
  nest_append_interval(ruleData, "days", 14) %>%
#  left_join(truthData, by = "AccountNumber") %>%
  group_by(AccountNumber)
toc()

cogsDataDFratio <- cogsData %>%
  nest_interval_unnest() %>%
  select(-ends_with("_N"), -ends_with("_IN"), -ends_with("DN"), 
         -matches("_A_"), -matches("_L_"), -matches("_R_"))

badColsratio <- names(which(unlist(lapply(cogsDataDFratio, function(X) all(is.na(X))))))

cogsDataDFratio <- cogsDataDFratio[,!(names(cogsDataDFratio) %in% badColsratio)]

cogsDataDFratio[is.na(cogsDataDFratio)] <- 0

