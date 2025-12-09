# Title: Functions building and interacting with the `sockeye` database
# Author: Ravi Maharaj
# Date: 2025-12-03

# Description ----
#' This script contains code used to:
#' - Build the `sockeye` database

# Setup environment ----
rm(list=ls())

# Libraries ----
require(rJava)
require(RJDBC)
require(DBI)
require(RSQLite)
require(tidyr)
require(dplyr)
require(purrr)

source("data-raw/kokanee.R")

#' Build `sockeye`
#' Build the sockeye database using raw data files in the `kokanee` data lake.
#'
#' @param db_path Target path for the `sockeye` database.
#' @param drv The database driver, RSQLite::SQLite used by default. Can be user-
#'            specified opath to local database driver.
#'
#' @returns The `sockeye` database object.

build_sockeye <- function(db_path = "data/sockeye/sockeye.db", 
                          drv = SQLite(),
                          driver_class = NULL) {
  
  sockeye_conn <- connect_sockeye(db_path = db_path,
                                  drv = drv,
                                  driver_class = driver_class)
  
  path <- "data/sockeye/KIT_BIODATA.csv"
  kitimat <- read.csv(path)
  kitimat_names <- names(kitimat)
  kitimat_data_types <- modify(as.list(kitimat), \(x) class(x)) |> 
    unlist()
  
  
  
  modify2(kitimat_names, kitimat_data_types, \(x, y) paste0(x, ""))
  
  
  Table1 <- dbExecute(conn, 
                      "CREATE TABLE Customers (
                                      customer_id INTEGER NOT NULL ,
                                      country_name VARCHAR(20) NOT NULL,                               
                                      country_code VARCHAR(20) NOT NULL
                                      )", 
                      errors=FALSE
  )
  
}




#' Connect `sockeye`
#' 
#' Connect to the `sockeye` database. If user specifies driver path and class, 
#' the function will also load the database driver to be used and create a JDBC
#' connection string.
#'
#' @param db_path Path of the `sockeye` database. Defaults to
#'                "data/sockeye/sockeye.db".
#' @param drv The database driver. Defaults to using RSQLite::SQLite().
#' @param driver_class The user-specified class of the database driver.
#'
#' @returns Connection to the `sockeye` database.
#'
#' @examples
connect_sockeye <- function(db_path = "data/sockeye/sockeye.db", 
                            drv = SQLite(),
                            driver_class) {
  
  if(class(drv)[1] != "SQLiteDriver") {
    
    if(is.null(driver_class)) {
      stop("'Driver class' must be specified if using a locally-stored database driver.")
    }
    
    drv <- JDBC(driver_class, drv)
    db_path <- file.path(getwd(), db_path)
    db_path <- paste0("jdbc:sqlite:", db_path)
  }
  
  return(dbConnect(drv, db_path))
  
}