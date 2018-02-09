library(tsCogs)


###  Read in Daily Profiles of eClaims submissions created in SQL table by Trenton  ###
sqlFilename = 'dbo.eClaimDailyProfilesFull'
cn = odbcDriverConnect("Driver={SQL Server Native Client 11.0};Server=hspsdata.nt.local;Database=SupportReports;Uid=USHSI/trenton.pulsipher;Pwd=22AngelA;trusted_connection=yes;",
                       believeNRows = F)
d = sqlFetch(cn, sqlFilename) # ~200 Mb sized object, ~13.4M rows

# load("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll-eClaims-20171201.Rdata")
# saveRDS(rawDailyProfilesAll, file = "rawDailyProfilesAll.rds")

# For home machine
rawData <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Development/R_Projects/hspsDS/R_Data/rawDailyProfilesAll.rds") %>%
  select(AccountNumber, AccountRowId, k200, Cluster, Cluster3, starts_with("x")) %>%
  gather(key = "Date", value = "Count", X2014.10.01:X2017.09.12) %>%
  mutate(Date = ymd(str_replace(Date, "X", "")),
         AccountNumber = as.integer(AccountNumber),
         Count = as.integer(Count))

rawData <- readRDS("~/R/R_prjs/tsCogs/R_Data/rawDailyProfilesAll.rds") %>%
  select(AccountNumber, AccountRowId, k200, Cluster, Cluster3, starts_with("x")) %>%
  gather(key = "Date", value = "Count", X2014.10.01:X2017.09.12) %>%
  mutate(Date = ymd(str_replace(Date, "X", "")),
         AccountNumber = as.integer(AccountNumber),
         Count = as.integer(Count))

smallData <- rawData %>%
  filter(Cluster3 == "Dropped")
         #AccountNumber %in% c(54070, 8888, 87805))
         #AccountNumber %in% sample(as.integer(unique(AccountNumber)), 10))
#filter(AccountNumber == 742)
# 25398
# ('49266', '80489', '60269', '67168', '51938', '1408', '77872', '86803', '18461',  '7366')
# 2017-09-12
# 2017-06-14

#########################################################

time1 <- Sys.time()

bob <- rawData %>%
  nest_todo()

Sys.time() - time1

bob %<>%
  nest_append_interval(rawData, "years", 1)

Sys.time() - time1

bob %<>%
  nest_append_interval(rawData, "months", 6)

Sys.time() - time1

bob %<>%
  nest_append_interval(rawData, "months", 3)

Sys.time() - time1

bob %<>%
  nest_append_interval(rawData, "weeks", 6)

Sys.time() - time1

bob %<>%
  nest_append_interval(rawData, "days", 14)

Sys.time() - time1

bob

Sys.time() - time1

bob <- nest_core(smallData, "month")


sc <- spark_connect(master = "local")
iris_tbl <- copy_to(sc, iris, "iris", overwrite = TRUE)

kmeans_model <- iris_tbl %>%
  ml_kmeans(~., centers = 100)

#########################################################

bob %>%
  ggplot(aes(x = Date, y = M_AVG, color = factor(AccountNumber))) +
  geom_line() +
  geom_hline(aes(yintercept = quant)) +
  facet_grid(AccountNumber~., scales = "free_y") +
  theme_bw()

#########################################################

rawData %>%
  filter(AccountNumber == "8888") %>%
  cut_Points() %>%
  ggplot(aes(x = Date, y = Count, color = factor(AccountNumber))) +
  geom_point() +
  geom_line(aes(x = Date, y = M_AVG), color = "blue") +
  geom_hline(aes(yintercept = quant)) +
  #facet_grid(AccountNumber~., scales = "free_y") +
  theme_bw()


