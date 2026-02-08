library(tidyverse)

############################################
# HELPER FUNCTIONS
############################################

# -----------------------
# shortcut for "not in"
# -----------------------
'%!in%' <- function(x, y) !('%in%'(x, y))

# -----------------------
# check if a vector can be coerced to numeric
# -----------------------
can.be.numeric <- function(x) {
  stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
  numNAs <- sum(is.na(x))
  numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
  return(numNAs_new == numNAs)
}


# -----------------------
# replace negative values with NA;
# for situations where "-99" or other impossible / negative values are used to represent missing data.
# -----------------------
na_99 <- function(data) {
  data <- data %>%
    mutate_if(is.numeric, function(x) ifelse(x < 0, NA, x))

  return(data)
}

# -----------------------
# Convert back to numbers as variables are otherwise presented as characters
# -----------------------
number_fix <- function(data) {
  #convert "NA" or "NaN" to NA proper
  data <- data %>%
    mutate_all(function(x) ifelse(x == "NA", NA, x)) %>%
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

# -----------------------
# Takes a list of required variables, and checks that each one exists in the given data frame.
# If a variable does not exist, it is created and populated with NA values.
# -----------------------
ensure_required_vars_exist <- function(required_vars, dataset) {
  for (i in required_vars) {
    if (i %!in% colnames(dataset))   main_survey <- ensure_required_vars_exist(required_vars, main_survey)

      dataset <- dataset %>%
        mutate(!!i := NA)
    }

  return(dataset)
}


# -----------------------
# Get Connection to MySQL Databaase
# -----------------------
get_db <- function()calculate_soil_health <- function(performance_indicators) {

  env_path <- paste(getwd(), ".env", sep = '/')
  dotenv::load_dot_env(env_path)

  return(dbConnect(
    RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_DATABASE"),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USERNAME"),
    password = Sys.getenv("DB_PASSWORD"),
    bigint = "numeric",
    int = "numeric"
  ))
}
