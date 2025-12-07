# Example file - generates a test csv output file
# This can be used to check that R can be run from CLI and from PHP via Symfony process.
library('dbplyr')
library('dplyr')
library(RMariaDB)
library(dotenv)

dotenv::load_dot_env(".env")

get_db <- function() {
    return(dbConnect(RMariaDB::MariaDB(),
                 dbname = Sys.getenv("DB_DATABASE"),
                 host = Sys.getenv("DB_HOST"),
                 port = as.integer(Sys.getenv("DB_PORT")),
                 user = Sys.getenv("DB_USERNAME"),
                 password = Sys.getenv("DB_PASSWORD")
    ))
}

# get connection to platform database
con <- get_db()

# get data from users table (to test it works)
users <- tbl(con, 'users') %>%
    select('id', 'email') %>%
    collect()


# export csv file
write.csv(users, "user-test.csv", row.names = FALSE)
