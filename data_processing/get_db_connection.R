library(RMariaDB)
library(ssh)
library(sys)
library(tidyverse)
library(jsonlite)
library(dotenv)

source("db_functions.R")

load_dot_env(".env")

ssh_command<-forge_tunnel(ip=Sys.getenv("IP"),ssh_priv_local=Sys.getenv("SSH_PRIV_LOCAL"))

pid <- sys::r_background(std_out = FALSE,
                         std_err = FALSE,
                         args = c("-e", ssh_command))

Sys.sleep(5)

conn <- dbConnect(
  RMariaDB::MariaDB(),
  
  password=Sys.getenv("DB_PASSWORD"),
  dbname=Sys.getenv("DB_DATABASE"),
  
  user=DB_USER,
  host=DB_HOST,
  port=DB_PORT)

data <- dbGetQuery(conn,"SELECT * FROM main_surveys")
