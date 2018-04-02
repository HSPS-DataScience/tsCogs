#' @title find_SignedSequence
#' @export
#' @description **Inteded to find largest sequence of negative, zero, or positive integers**
#'
#' @param data Must be vector of numbers
#' @param number Must be one of following integers:
#' * `-1`
#' * `0`
#' * `1`
#'
#' @return integer
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

#' @title find_LadderSequence
#' @export
#' @description **Inteded to find largest sequence of many ladder sequences**
#'
#' @param data Must be vector of numbers
#' @param sequence Must be one of following strings:
#' * `I` - Increasing
#' * `D` - Decreasing
#' * `IP` - Increasing Positive
#' * `P` - Decreasing Positive
#' * `IN` - Increasing Negative
#' * `DN` - Decreasing Negative
#'
#' @return integer
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
