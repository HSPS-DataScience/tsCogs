library(tsCogs)

# Preparation
tic()
bob <- rawData %>%
  filter(AccountNumber %in% c("75815", "15300")) %>%
  as.tbl() %>%
  mutate(AccountNumber = as.character(AccountNumber)) %>%
  rename(Date = ymd) %>%
  group_by(AccountNumber) %>%
  filter(!is.na(AccountNumber),
         Date >= ymd("2014-11-01"),
         Date <= ymd("2018-02-24")) %>%
  arrange(Date)
toc()

# Cut Point
tic()
cutData <- bob %>% 
  cut_point()
toc()

# Rules
tic()
cutData %<>%
  group_by(AccountNumber) %>%
  mutate(totalCount = sum(Count),
         numDays = n()) %>%
  filter(totalCount >= 600,
         numDays >= 90)
toc()

# Store valid Accounts
keepIDs <- cutData %>%
  pull(AccountNumber) %>%
  unique()

# Cluster
tic()
clusterData <- bob %>%
  ungroup() %>%
  filter(AccountNumber %in% keepIDs) %>%
  normalize_weekly() %>%
  kMeans_sparkly(centers = 100) # seed is set inside the function
toc()

# Truth
truthData <- clusterData %>%
  select(AccountNumber, prediction) %>%
  mutate(Truth = "Healthy") %>%
  #mutate(Truth = if_else(prediction %in% c(1:5,7,8,14,15,17,18,20,21,26,28,29,
  #                                         31,32,34,35,37,38,40,45,49,51,54,56,57,59,
  #                                         60,62:65,57:70,72,75:77,80,84,85,87,
  #                                         94,98,99), "Healthy", "Dropped")) %>%
  select(AccountNumber, prediction, Truth) %>%
  group_by(AccountNumber) %>%
  slice(1)

# Cogs and Rejoin
jack <- cutData %>%
  nest_todo() %>%
  nest_append_interval(cutPtData, "years", 1) %>%
  nest_append_interval(cutPtData, "months", 6) %>%
  nest_append_interval(cutPtData, "months", 3) %>%
  nest_append_interval(cutPtData, "weeks", 6) %>%
  nest_append_interval(cutPtData, "days", 14) %>%
  left_join(truthData, by = "AccountNumber")
  