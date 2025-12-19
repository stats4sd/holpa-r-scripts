
## HELPER FUNCTIONS

# ** shortcut for "not in"
'%!in%' <- function(x,y)!('%in%'(x,y))

# ** check if a vector can be coerced to numeric
can.be.numeric <- function(x) {
  stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
  numNAs <- sum(is.na(x))
  numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
  return(numNAs_new == numNAs)
}


# ** replace negative values with NA; for situations where "-99" or other impossible / negative values are used to represent missing data.
na_99 <- function(data){

  data <- data%>%
    mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x))

  return(data)

}

# ** Convert back to numbers as variables are otherwise presented as characters
number_fix <- function(data){

  #convert "NA" or "NaN" to NA proper
  data <- data%>%
    mutate_all(function(x) ifelse(x == "NA", NA, x))%>%
    mutate_all(function(x) ifelse(x == "NaN", NA, x))

  data <- as.data.frame(lapply(data, function(col) {
    if (can.be.numeric(col)) {
      as.numeric(col)
    } else {
      col
    }
  }))

  data <- na_99(data)

  return(data)

}
