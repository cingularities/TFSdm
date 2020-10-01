library(testthat)
library(TFSDataMonitoring)

se_file <- system.file("insta/extdata", "se.csv")
se <- read.csv(se_file)


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
    expect_equal(ncol(dm_down(se, sma, sp, status, clients)), 10)
  }
)

#> Test passed
