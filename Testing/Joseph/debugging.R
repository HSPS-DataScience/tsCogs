library(tsCogs)

# Preparation
tic()
bob <- rawData %>%
  #filter(AccountNumber %in% c("75815", "15300")) %>%
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






##### Testing ####

joe <- cutData %>%
  #filter(AccountNumber %in% c("000040"))
  filter(AccountNumber %in% c("000019", "000040"))

cogsData <- cutData %>%
  nest_todo() %>%
  nest_append_interval(cutData, "years", 1) %>%
  nest_append_interval(cutData, "months", 6) %>%
  nest_append_interval(cutData, "months", 3) %>%
  nest_append_interval(cutData, "weeks", 6) %>%
  nest_append_interval(cutData, "days", 14) %>%
  left_join(truthData, by = "AccountNumber") %>%
  group_by(AccountNumber)


#### New Big Checking ####

bob <- cutData %>%
#cutData %>%
  filter(AccountNumber %in% c('001307', '001354', '8888')) %>%
  select(AccountNumber, Date, Count) %>%
  group_by(AccountNumber) #%>%
  filter(min(Date) >= max(Date) - years(1)) %>%
  slice(1) %>%
  pull(AccountNumber)
  # filter_at(vars("Date"),
  #           any_vars(. <= max(.) - lubridate::years(1))) %>%
  pull(AccountNumber)
  #testFun()
  
bob %>%
  nest_todo() %>%
  nest_append_interval_test(bob, "years", 1)
  #nest_core_interval_test("years", 1, "A")
  
  

  
  nest_append_interval_test <- function(nestTib, rawData, type, interval) {
    
    organizedData <- nest_interval_test(rawData, type, interval)
    
    nestTib %>%
      left_join(organizedData, by = "AccountNumber")
  }  

  
  nest_interval_test <- function(data, type, interval) {
    
    derefType <- "type"
    
    typeCapital <- capitalize(type)
    letter <- capitalize(substr(type, 1, 1))
    
    allData <- data %>%
      select(AccountNumber, Date, Count) %>%
      group_by(AccountNumber) %>%
      filter(Date %within% ((max(Date) - do.call(get(derefType), list(interval * 2)))
                            %--% max(Date)))
    
    rightData <- allData %>%
      filter(Date >= ((max(Date) - do.call(get(derefType), list(interval))))) %>%
      nest_core_interval_test(type, interval, "R")
    
    leftData <- allData %>%
      filter(Date < ((max(Date) - do.call(get(derefType), list(interval))))) %>%
      nest_core_interval_test(type, interval, "L")
    
    ratioData <- left_join(leftData, rightData, by = "AccountNumber")
    
    ratioData %<>%
      mutate(
        !!paste0(letter, interval, "_Ratios"):= map2(ratioData[[2]],
                                                     ratioData[[3]],
                                                     ~ as_tibble(.y/.x) %>%
                                                       rename_all(funs(sub('_.', "_Ratio", .))))) %>%
      select(AccountNumber, contains("Ratio"))
    
    allData %<>%
      nest_core_interval_test(type, interval, "A")
    
    left_join(allData, leftData, by = "AccountNumber") %>%
      left_join(., rightData, by = "AccountNumber") %>%
      left_join(., ratioData, by = "AccountNumber")
  }
  
nest_core_interval_test <- function(data, type, interval, divide) {
  
  derefType <- "type"
  derefInterval <- "interval"
  
  letter <- capitalize(substr(type, 1, 1))
  
  print(length(unique(data$AccountNumber)))
  
  print(max(data$Date))
  
  print(min(data$Date))
  
  print(unique(data$AccountNumber))
  
  #nullAccounts <- data %>%
  data %<>%
    #filter(AccountNumber %in% c('001307', '001354')) %>%
    select(AccountNumber, Date, Count) %>%
    group_by(AccountNumber) %>%
    filter(min(Date) <= max(Date) - do.call(get(derefType), 
                                            list(get(derefInterval))))
    #slice(1) %>%
    #pull(AccountNumber)
  
  #print(nullAccounts)
  
  # if(data %>%
  #    group_by(AccountNumber) %>%
  #    slice(1) %>%
  #    pull(AccountNumber) %in% nullAccounts) {
  #   print()
  # }
  
  print(length(unique(data$AccountNumber)))
  
  data %>%
    select(AccountNumber, Date, Count) %>%
    ungroup() %>%
    partition(AccountNumber) %>%
    summarise(!!paste0(letter, interval, "_", divide, "_Count") := sum(Count),
              !!paste0(letter, interval, "_", divide, "_Mean") := mean(Count),
              !!paste0(letter, interval, "_", divide, "_Median") := median(Count),
              !!paste0(letter, interval, "_", divide, "_SD") := sd(Count),
              !!paste0(letter, interval, "_", divide, "_Max") := max(Count),
              !!paste0(letter, interval, "_", divide, "_Min") := min(Count),
              !!paste0(letter, interval, "_", divide, "_CV") := (sd(Count) / mean(Count)),
              
              #######################################################################
              
              #!!paste0(letter, interval, "_", divide, "_SLP") := (lm(Count ~ as.numeric(Date),
              #                                                       data = .)[["coefficients"]][2]),
              !!paste0(letter, interval, "_", divide, "_OOC2") := (sum(Count >= (mean(Count) + (2 * sd(Count))))),
              !!paste0(letter, interval, "_", divide, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count))))),
              
              #######################################################################
              
              !!paste0(letter, interval, "_", divide, "_P") := tsCogs::find_SignedSequence(Count, 1),
              !!paste0(letter, interval, "_", divide, "_N") := tsCogs::find_SignedSequence(Count, -1),
              !!paste0(letter, interval, "_", divide, "_Z") := tsCogs::find_SignedSequence(Count, 0),
              
              #######################################################################
              
              !!paste0(letter, interval, "_", divide, "_I") := tsCogs::find_LadderSequence(Count, "I"),
              !!paste0(letter, interval, "_", divide, "_D") := tsCogs::find_LadderSequence(Count, "D"),
              !!paste0(letter, interval, "_", divide, "_IP") := tsCogs::find_LadderSequence(Count, "IP"),
              !!paste0(letter, interval, "_", divide, "_DP") := tsCogs::find_LadderSequence(Count, "DP"),
              !!paste0(letter, interval, "_", divide, "_IN") := tsCogs::find_LadderSequence(Count, "IN"),
              !!paste0(letter, interval, "_", divide, "_DN") := tsCogs::find_LadderSequence(Count, "DN")
    ) %>%
    collect() %>%
    group_by(AccountNumber) %>%
    #mutate_if(.$AccountNumber %in% nullAccounts, NULL) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, interval, "_", divide, "_Cognostics") := Cogs)
}


  

##########################
  
bob <- trent %>%
  nest_interval_unnest()

