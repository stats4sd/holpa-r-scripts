library(RMariaDB)
library(ssh)
library(sys)
library(tidyverse)
library(jsonlite)
library(dotenv)
library(data.table)

'%!in%' <- function(x,y)!('%in%'(x,y))

env_path <- paste(getwd(), ".env", sep = '/')

dotenv::load_dot_env(env_path)

get_db <- function() {
  return(dbConnect(RMariaDB::MariaDB(),
                   dbname = Sys.getenv("DB_DATABASE"),
                   host = Sys.getenv("DB_HOST"),
                   port = as.integer(Sys.getenv("DB_PORT")),
                   user = Sys.getenv("DB_USERNAME"),
                   password = Sys.getenv("DB_PASSWORD"),
                   bigint = "numeric",
                   int = "numeric"

  ))
}

con <- get_db()


