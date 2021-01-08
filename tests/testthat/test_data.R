context("Data loading and column count")

library(testthat)
library(TFSDataMonitoring)
library(dplyr)
library(fuzzyjoin)
library(plyr)
library(readr)



test_that("Locating data file in package",
          {
  test_data_path <- system.file("insta/extdata", "se.csv", package="TFSDataMonitoring")
  expect_true( file.exists(test_data_path) )
  }
)

#> Test passed

test_that(
  "Checking column result expectations",
  {
    se_file<- read_csv("insta/extdata/se.csv")
    se<- readr::read_csv(se_file)
    sma_file <- system.file("insta/extdata", "sma.csv")
    sma<- readr::read_csv(sma_file)
    sp_file <- system.file("insta/extdata", "sp.csv")
    sp<- readr::read_csv(sp_file)
    status_file <- system.file("insta/extdata", "status.csv")
    status<- readr::read_csv(status_file)
    clients_file <- system.file("insta/extdata", "clients.csv")
    clients<- readr::read_csv(clients_file)

    expect_equal(ncol(dm_down(se, sma, sp, status, clients)), 10)
  }
)

#> Test passed

test_that(
  "Checking column result expectations",
  {
    se<- system.file("insta/extdata", "se.csv")
    sma <- system.file("insta/extdata", "sma.csv")
    sp<- system.file("insta/extdata", "sp.csv")
    status <- system.file("insta/extdata", "status.csv")
    clients <- system.file("insta/extdata", "clients.csv")

    expect_equal(ncol(dm_down(se, sma, sp, status, clients)), 10)
  }
)

#> Test passed
