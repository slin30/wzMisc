library(testthat)
library(wzMisc)

#create if not exists testthat\testdata
if(!dir.exists("./tests/testdata")) {
  dir.create("./tests/testdata", recursive = TRUE)
}

test_check("wzMisc")
