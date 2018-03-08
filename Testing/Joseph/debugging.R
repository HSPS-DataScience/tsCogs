library(tsCogs)

# Preparation
tic()
bob <- rawData %>%
  #filter(AccountNumber %in% c("75815", "15300")) %>%
#  filter(AccountNumber %in% c("75815", "15300")) %>%
  as.tbl() %>%
  mutate(AccountNumber = as.character(AccountNumber)) %>%
  rename(Date = ymd) %>%
  group_by(AccountNumber) %>%
  filter(!is.na(AccountNumber),
         Date >= ymd("2014-11-01"),
         Date <= ymd("2018-02-24")) %>%
  arrange(Date)


# Cut Point
cutData <- bob %>% 
  cut_point()


# Rules
cutData %<>%
  group_by(AccountNumber) %>%
  mutate(totalCount = sum(Count),
         numDays = n()) %>%
  filter(totalCount >= 600,
         numDays >= 90) %>%
  select(-tmp_reverseSign, -tmp_cumSum)

# Store valid Accounts
keepIDs <- cutData %>%
  pull(AccountNumber) %>%
  unique()

# Cluster
clusterData <- bob %>%
  ungroup() %>%
  filter(AccountNumber %in% keepIDs) %>%
  normalize_weekly() %>%
  kMeans_sparkly(centers = 100) # seed is set inside the function

# Truth
truthData <- clusterData %>%
  select(AccountNumber, prediction) %>%
  #mutate(Truth = "Healthy") %>%
  mutate(Truth = if_else(prediction %in% c(1:5,7,8,14,15,17,18,20,21,26,28,29,
                                           31,32,34,35,37,38,40,45,49,51,54,56,57,59,
                                           60,62:65,57:70,72,75:77,80,84,85,87,
                                           94,98,99), "Healthy", "Dropped")) %>%
  select(AccountNumber, prediction, Truth) %>%
  group_by(AccountNumber) %>%
  slice(1)

rm(bob, keepIDs, clusterData)

toc()

# Cogs and Rejoin
tic()
jack <- cutData %>%
  nest_todo() %>%
  nest_append_interval(cutData, "years", 1) %>%
  nest_append_interval(cutData, "months", 6) %>%
  nest_append_interval(cutData, "months", 3) %>%
  nest_append_interval(cutData, "weeks", 6) %>%
  nest_append_interval(cutData, "days", 14) %>%
  left_join(truthData, by = "AccountNumber")
toc()


# Multidplyr
library(multidplyr)

cluster_library()

tic()
jack <- cutData %>%
  nest_todo()
toc()








nest_core_test <- function(data, type) {
  
  tmpColName <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))
  
  data %>%
    #filter(AccountNumber != 0) %>%
    select(AccountNumber, Date, Count) %>%
    group_by(AccountNumber, floor_date(Date, type)) %>%
    summarise(Count = sum(Count)) %>%
    #partition(AccountNumber) %>%
    group_by(AccountNumber) %>%
    summarise(!!paste0(letter, "_Count") := sum(Count),
              !!paste0(letter, "_Mean") := mean(Count),
              !!paste0(letter, "_Median") := median(Count),
              !!paste0(letter, "_SD") := sd(Count),
              !!paste0(letter, "_Max") := max(Count),
              !!paste0(letter, "_Min") := min(Count),
              !!paste0(letter, "_CV") := (sd(Count) / mean(Count)),

              #######################################################################

              # !!paste0(letter, "_SLP") := (lm(Count ~ as.numeric(tmpColName),
              #                                 data = .)[["coefficients"]][2]),
              !!paste0(letter, "_OOC2") := (sum(Count >= (mean(Count) + (2 * sd(Count))))),
              !!paste0(letter, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count))))),

              #######################################################################

              !!paste0(letter, "_P") := find_SignedSequence(Count, 1),
              !!paste0(letter, "_N") := find_SignedSequence(Count, -1),
              !!paste0(letter, "_Z") := find_SignedSequence(Count, 0),

              #######################################################################

              !!paste0(letter, "_I") := find_LadderSequence(Count, "I"),
              !!paste0(letter, "_D") := find_LadderSequence(Count, "D"),
              !!paste0(letter, "_IP") := find_LadderSequence(Count, "IP"),
              !!paste0(letter, "_DP") := find_LadderSequence(Count, "DP"),
              !!paste0(letter, "_IN") := find_LadderSequence(Count, "IN"),
              !!paste0(letter, "_DN") := find_LadderSequence(Count, "DN")
    ) %>%
    #collect() %>%
    group_by(AccountNumber) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, "_Cognostics") := Cogs)
}
=======
  left_join(truthData, by = "AccountNumber") %>%
  group_by(AccountNumber)
  
>>>>>>> c3d5b1a5fd1778a826b596178892a42ea250c19b
