#' find_SignedSequence
#'
#' @param data Must be vector of numbers
#' @param number Must be one of following integers:
#' 1. -1 -- Negative Integers
#' 2. 0 -- Zero Integers
#' 3. 1 -- Positive Integers
#'
#' @return integer
#' @export
#'
#' @examples test
find_SignedSequence <- function(data, number) {

  if(length(rle(sign(data))[[1]][rle(sign(data))[[2]] == number]) >= 1) {
    return(max(rle(sign(data))[[1]][rle(sign(data))[[2]] == number]))
  } else {
    return(NA)
  }
}

######################################################################

#' find_LadderSequence
#'
#' @param data Must be vector of numbers
#' @param sequence Must be one of following strings:
#' 1. "I" -- Increasing
#' 2. "D" -- Decreasing
#' 3. "IP" -- Increasing Positive
#' 4. "DP" -- Decreasing Positive
#' 5. "IN" -- Increasing Negative
#' 6. "DN" -- Decreasing Negative
#'
#' @return integer
#' @export
#'
#' @examples test
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
