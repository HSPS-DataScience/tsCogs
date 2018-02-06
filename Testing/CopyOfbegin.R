library(tidyverse)
library(trelliscope)
library(trelliscopejs)
library(magrittr)
library(lubridate)
library(rbokeh)
library(Hmisc)
library(profvis)
library(purrr)
library(pracma)
library(strucchange)
library(trelliscopejs)


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

# #smallData <- rawData %>%
# bob <- rawData %>%
#   select(AccountNumber, Date) %>%
#   group_by(AccountNumber) %>%
#   slice(which.min(Date)) %>%
#   rename(Min_Date = Date) %>%
#   filter(Min_Date < ymd("2017-06-14"))

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





################################################################################
#                           TESTING_ENVIORNMENT
#                               CUT_POINT
################################################################################

breakDate_end <- function(data = data) {
  if(sum(data$Count) == 0) {
    dateEnd <- data %>%
      arrange(Date) %>%
      slice(n()) %>%
      pull(Date)
  } else {
    tmp = breakpoints(M_AVG ~ Date, data = data)
    if(length(tmp$breakpoints) > 0) {
      tmpIndex <- tail(tmp$breakpoints, 1)
      
      dateEnd <- data %>%
        arrange(Date) %>%
        slice(tmpIndex) %>%
        pull(Date)
    } else {
      dateEnd <- data %>%
        arrange(Date) %>%
        slice(n()) %>%
        pull(Date)
    }
  }
  data %>%
    filter(Date <= dateEnd) %>%
    as_tibble()
}

breakDate_start <- function(data = data) {
  data %>%
    filter(M_AVG > quant) %>%
    pull(Date) %>%
    min(na.rm = FALSE)
}

<<<<<<< HEAD
bob <- smallData %>%
#smallData %>%
#cutPoint <- smallData %>%
  filter(AccountNumber %in% c(64220)) %>%
  group_by(AccountNumber) %>%
  mutate(M_AVG = movavg(Count, 21, "s"),
         tmp_reverseSign = if_else(M_AVG > 0, 0, 1, missing = NULL),
         tmp_cumSum = cumsum(tmp_reverseSign),
         quant = quantile(M_AVG[M_AVG > 0], probs = 0.2)) %>%
  filter(Date >= breakDate_start(.)) #%>%
  split(.$AccountNumber) %>%
  map(~ breakDate_end(.)) #%>%

  
##  Trenton's attempt ##
time1 <- Sys.time()

accountData <- smallData %>%
  filter(AccountNumber %in% c(64220)) %>%
  group_by(AccountNumber) %>%
  mutate(M_AVG = movavg(Count, 21, "s"),
         quant = quantile(M_AVG[M_AVG > 0], probs =  0.2),
         startDate = min(Date[M_AVG > quant]))


cutPoint <- accountData %>%
  filter(AccountNumber %in% c(64220)) %>%
  #filter(AccountNumber %in% c(64220, 14876, 89524)) %>%
  group_by(AccountNumber) %>%
  # calculate startDate #
  mutate(M_AVG = movavg(Count, 21, "s"),
         quant = quantile(M_AVG[M_AVG > 0], probs = 0.2),
         startDate = min(Date[(M_AVG > quant)])) %>%
  filter(Date >= startDate) %>%
  # calculate temporary endDate of data (remove some trailing zeros)#
  mutate(tmp_reverseSign = if_else(M_AVG > 0, 0, 1, missing = NULL),
         tmp_cumSum = cumsum(tmp_reverseSign),
         zeroDate = max(Date[tmp_cumSum < 10])) %>%
  filter(Date <= zeroDate) %>%
  # calculate true cutPoint/endDate date based on newly trimmed data #
  mutate(endDate = Date[tail(breakpoints(M_AVG ~ Date)$breakpoints,1)]) %>%
  filter(Date <= endDate) #%>%

Sys.time() - time1

cutPoint %>%
  ggplot(aes(x = Date, y = M_AVG)) +
  geom_line() +
  geom_line(aes(x = Date, y = M_AVG), data = accountData) +
  theme_bw()
    # ggplot(aes(x = Date, y = Count, color = factor(AccountNumber))) +
=======
# bob <- smallData %>%
# #smallData %>%
# #cutPoint <- smallData %>%
#   filter(AccountNumber %in% c(64220)) %>%
#   group_by(AccountNumber) %>%
#   mutate(M_AVG = movavg(Count, 21, "s"),
#          tmp_reverseSign = if_else(M_AVG > 0, 0, 1, missing = NULL),
#          tmp_cumSum = cumsum(tmp_reverseSign),
#          quant = quantile(M_AVG[M_AVG > 0], probs = 0.2)) %>%
#   filter(Date >= breakDate_start(.)) #%>%
#   split(.$AccountNumber) %>%
#   map(~ breakDate_end(.)) #%>%
# 
# 
# tibble(
#   pair = map(lol, "pair"),
#   genes_vec = map_chr(lol, "genes")
# ) %>% 
#   mutate(
#     pair1 = map_chr(pair, 1),
#     pair2 = map_chr(pair, 2) 
#   ) %>%
#   select(pair1, pair2, genes_vec)
  
  
  
##  Trenton's attempt (took 4 hours to run)  ##
time1 <- Sys.time()
  cutPoint <- smallData %>%
    # filter(AccountNumber %in% c(64220, 14876, 89524)) %>%
    group_by(AccountNumber) %>%
    # calculate startDate #
    mutate(M_AVG = movavg(Count, 21, "s"),
           quant = quantile(M_AVG[M_AVG > 0], probs = 0.2),
           startDate = min(Date[(M_AVG > quant)])) %>%
    filter(Date >= startDate) %>%
    # calculate temporary endDate of data (remove some trailing zeros)#
    mutate(tmp_reverseSign = if_else(M_AVG > 0, 0, 1, missing = NULL),
           tmp_cumSum = cumsum(tmp_reverseSign),
           zeroDate = max(Date[tmp_cumSum < 10])) %>%
    filter(Date <= zeroDate) %>%
    # calculate true cutPoint/endDate date based on newly trimmed data #
    mutate(endDate = Date[tail(breakpoints(M_AVG ~ Date)$breakpoints,1)]) %>%
    filter(Date <= endDate) #%>%
Sys.time() - time1
saveRDS(cutPoint, "~/R/R_prjs/hspsDS/R_Data/cutPoint.rds")

# ggplot(aes(x = Date, y = Count, color = factor(AccountNumber))) +
>>>>>>> 9da5f7ffe5ca884e51091c00e4ae1c0f9115bfd6
    #   geom_line(colour = "black") +
    #   geom_line(aes(y = M_AVG), size = I(1.5)) +
    #   # geom_hline(aes(yintercept = quant)) +
    #   geom_vline(aes(xintercept = startDate)) +
    #   geom_vline(aes(xintercept = endDate)) +
    #   geom_vline(aes(xintercept = zeroDate), linetype = 3) +
    #   facet_grid(AccountNumber~., scales = "free_y") +
    #   theme_bw() +
    #   labs(x = "") +
    #   theme(legend.position = "none")
  
  
<<<<<<< HEAD
  unnest()
  ggplot(aes(x = Date, y = M_AVG, color = factor(AccountNumber))) +
  geom_line() +
  geom_hline(aes(yintercept = quant)) +
  facet_grid(AccountNumber~., scales = "free_y") +
  theme_bw()

bob %>%
  ggplot(aes(x = Date, y = M_AVG, color = factor(AccountNumber))) +
  geom_line() +
  geom_hline(aes(yintercept = quant)) +
  facet_grid(AccountNumber~., scales = "free_y") +
  theme_bw()

=======
>>>>>>> 85711505a30a6617f19ef803c35c6e308bc9c7aa
  

################################################################################

################################################################################
#                           nest_todo
################################################################################

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

################################################################################
#                           nest_core
################################################################################

nest_core <- function(data, type) {
  
  tmpColName <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))
  
  data %>% 
    filter(AccountNumber != 0) %>%
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
    group_by(AccountNumber) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, "_Cognostics") := Cogs)
}

################################################################################
#                           nest_append_interval
################################################################################

nest_append_interval <- function(nestTib, rawData, type, interval) {
  
  organizedData <- nest_interval(rawData, type, interval)
  
  nestTib %>%
    left_join(organizedData, by = "AccountNumber")
}

################################################################################
#                           nest_interval
################################################################################

nest_interval <- function(data, type, interval) {
  
  derefType <- "type"
  
  typeCapital <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))
  
  allData <- data %>% 
    select(AccountNumber, Date, Count) %>%
    filter(AccountNumber != 0,
           Date %within% ((max(Date) - do.call(get(derefType), list(interval * 2))) 
                          %--% max(Date))) %>%
    group_by(AccountNumber, tmpColName = cut(Date, 2))
  
  rightData <- allData %>%
    ungroup() %>%
    filter(Date >= ((max(Date) - do.call(get(derefType), list(interval)))) - 1) %>%
    nest_core_interval(type, interval, "R")
  
  leftData <- allData %>%
    ungroup() %>%
    filter(Date < ((max(Date) - do.call(get(derefType), list(interval)))) - 1) %>%
    nest_core_interval(type, interval, "L")
  
  ratioData <- left_join(leftData, rightData, by = "AccountNumber")
  
  ratioData %<>%
    mutate(
      !!paste0(letter, interval, "_Ratios"):= map2(ratioData[[2]],
                                                   ratioData[[3]],
                                                   ~ as_tibble(.y/.x) %>%
                                                     rename_all(funs(sub('_.', "_Ratio", .))))) %>%
    select(AccountNumber, contains("Ratio"))
  
  allData %<>%
    ungroup() %>%
    nest_core_interval(type, interval, "A")
  
  left_join(allData, leftData, by = "AccountNumber") %>%
    left_join(., rightData, by = "AccountNumber") %>%
    left_join(., ratioData, by = "AccountNumber") %>%
    group_by(AccountNumber)
}

################################################################################
#                           nest_core_interval
################################################################################

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
              !!paste0(letter, interval, "_", divide, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count))))),
              
              #######################################################################
              
              !!paste0(letter, interval, "_", divide, "_P") := find_SignedSequence(Count, 1),
              !!paste0(letter, interval, "_", divide, "_N") := find_SignedSequence(Count, -1),
              !!paste0(letter, interval, "_", divide, "_Z") := find_SignedSequence(Count, 0),
              
              #######################################################################
              
              !!paste0(letter, interval, "_", divide, "_I") := find_LadderSequence(Count, "I"),
              !!paste0(letter, interval, "_", divide, "_D") := find_LadderSequence(Count, "D"),
              !!paste0(letter, interval, "_", divide, "_IP") := find_LadderSequence(Count, "IP"),
              !!paste0(letter, interval, "_", divide, "_DP") := find_LadderSequence(Count, "DP"),
              !!paste0(letter, interval, "_", divide, "_IN") := find_LadderSequence(Count, "IN"),
              !!paste0(letter, interval, "_", divide, "_DN") := find_LadderSequence(Count, "DN")
    ) %>%
    group_by(AccountNumber) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, interval, "_", divide, "_Cognostics") := Cogs)
}

################################################################################
#                           find_SignedSequence
################################################################################

find_SignedSequence <- function(data, number) {
  if(length(rle(sign(data))[[1]][rle(sign(data))[[2]] == number]) >= 1) {
    return(max(rle(sign(data))[[1]][rle(sign(data))[[2]] == number]))
  } else {
    return(NA)
  }
}

################################################################################
#                           find_LadderSequence
################################################################################

find_LadderSequence <- function(data, sequence) {
  
  max_Inc_Seq <- 
    max_Dec_Seq <- 
    max_Inc_Pos_Seq <-
    max_Dec_Pos_Seq <-
    max_Inc_Neg_Seq <-
    max_Dec_Neg_Seq <- NA
  
  poten_Max_Inc_Seq <- 
    poten_Max_Dec_Seq <- 
    poten_Max_Inc_Pos_Seq <-
    poten_Max_Dec_Pos_Seq <-
    poten_Max_Inc_Neg_Seq <-
    poten_Max_Dec_Neg_Seq <- 0
  
  for (i in seq_along(data)) {
    if(!is.na(data[i + 1])) {
      # Increasing
      if(data[i] < data[i + 1]) {
        poten_Max_Dec_Seq <- 0
        if(is.na(max_Inc_Seq)) {
          max_Inc_Seq <- 0
          max_Inc_Seq <- poten_Max_Inc_Seq <- poten_Max_Inc_Seq + 1
        }
        else {
          poten_Max_Inc_Seq <- poten_Max_Inc_Seq + 1
          if(poten_Max_Inc_Seq > max_Inc_Seq) {
            max_Inc_Seq <- poten_Max_Inc_Seq
          }
        }
        # Increasing Positive
        if(sign(data[i + 1]) == 1) {
          poten_Max_Dec_Pos_Seq <- 0
          if(is.na(max_Inc_Pos_Seq)) {
            max_Inc_Pos_Seq <- 0
            max_Inc_Pos_Seq <- poten_Max_Inc_Pos_Seq <- poten_Max_Inc_Pos_Seq + 1
          }
          else {
            poten_Max_Inc_Pos_Seq <- poten_Max_Inc_Pos_Seq + 1
            if(poten_Max_Inc_Pos_Seq > max_Inc_Pos_Seq) {
              max_Inc_Pos_Seq <- poten_Max_Inc_Pos_Seq
            }
          }
        }
        # Increasing Negative
        else if(sign(data[i + 1]) != 1) {
          poten_Max_Dec_Neg_Seq <- 0
          if(is.na(max_Inc_Neg_Seq)) {
            max_Inc_Neg_Seq <- 0
            max_Inc_Neg_Seq <- poten_Max_Inc_Neg_Seq <- poten_Max_Inc_Neg_Seq + 1
          }
          else {
            poten_Max_Inc_Neg_Seq <- poten_Max_Inc_Neg_Seq + 1
            if(poten_Max_Inc_Neg_Seq > max_Inc_Neg_Seq) {
              max_Inc_Neg_Seq <- poten_Max_Inc_Neg_Seq
            }
          }
        }
      }
      # Decreasing
      else if(data[i] > data[i + 1]) {
        poten_Max_Inc_Seq <- 0
        if(is.na(max_Dec_Seq)) {
          max_Dec_Seq <- 0
          max_Dec_Seq <- poten_Max_Dec_Seq <- poten_Max_Dec_Seq + 1
        }
        else {
          poten_Max_Dec_Seq <- poten_Max_Dec_Seq + 1
          if(poten_Max_Dec_Seq > max_Dec_Seq) {
            max_Dec_Seq <- poten_Max_Dec_Seq
          }
        }
        # Decreasing Positive
        if(sign(data[i + 1]) != -1) {
          poten_Max_Inc_Pos_Seq <- 0
          if(is.na(max_Dec_Pos_Seq)) {
            max_Dec_Pos_Seq <- 0
            max_Dec_Pos_Seq <- poten_Max_Dec_Pos_Seq <- poten_Max_Dec_Pos_Seq + 1
          }
          else {
            poten_Max_Dec_Pos_Seq <- poten_Max_Dec_Pos_Seq + 1
            if(poten_Max_Dec_Pos_Seq > max_Dec_Pos_Seq) {
              max_Dec_Pos_Seq <- poten_Max_Dec_Pos_Seq
            }
          }
        }
        # Decreasing Negative
        else if(sign(data[i + 1]) != 1) {
          poten_Max_Inc_Neg_Seq <- 0
          if(is.na(max_Dec_Neg_Seq)) {
            max_Dec_Neg_Seq <- 0
            max_Dec_Neg_Seq <- poten_Max_Dec_Neg_Seq <- poten_Max_Dec_Neg_Seq + 1
          }
          else {
            poten_Max_Dec_Neg_Seq <- poten_Max_Dec_Neg_Seq + 1
            if(poten_Max_Dec_Neg_Seq > max_Dec_Neg_Seq) {
              max_Dec_Neg_Seq <- poten_Max_Dec_Neg_Seq
            }
          }
        }
      }
    }
  }
  # Determining return statement
  if(sequence == "I") {
    return(max_Inc_Seq)
  }
  else if(sequence == "D") {
    return(max_Dec_Seq)
  }
  else if(sequence == "IP") {
    return(max_Inc_Pos_Seq)
  } 
  else if(sequence == "DP") {
    return(max_Dec_Pos_Seq)
  }
  else if(sequence == "IN") {
    return(max_Inc_Neg_Seq)
  } 
  else if(sequence == "DN") {
    return(max_Dec_Neg_Seq)
  }
}

