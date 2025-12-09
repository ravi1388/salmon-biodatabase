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
#' @param driver_path Path of the database driver.
#'
#' @returns The `sockeye` database object.
#'
#' @examples
build_sockeye <- function(db_path = "data/sockeye/sockeye.db", 
                          driver_path = SQLite()) {
  
  sockeye_conn <- dbConnect(driver_path, db_path)
  
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
#' Connect to the `sockeye` database
#'
#' @param db_path Path of the `sockeye` database. Defaults to
#'                "data/sockeye/sockeye.db".
#' @param driver_path Path of the database driver. Defaults to
#'                    "data/sockeye/dbdrivers/sqlite-jdbc-3.27.2.1.jar"
#' @param driver_class Class of the database driver. Defaults to "org.sqlite.JDBC"
#'
#' @returns Connection to the `sockeye` database.
#'
#' @examples
connect_sockeye <- function(db_path = "data/sockeye/sockeye.db", 
                            driver_path = "data/sockeye/dbdrivers/sqlite-jdbc-3.27.2.1.jar",
                            driver_class = "org.sqlite.JDBC") {
  
  # Connect to database ----
  #' - Specify database driver to be used
  #' - Create JDBC connection string
  #' - Connect to database
  
  jdbc_driver <- JDBC(driver_class, driver_path)
  jdbc_path <- file.path(getwd(), db_path)
  jdbc_path <- paste0("jdbc:sqlite:", jdbc_path)
  return(dbConnect(jdbc_driver, jdbc_path))
}