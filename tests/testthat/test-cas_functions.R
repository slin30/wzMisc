context("cas_functions")


# Test items --------------------------------------------------------------

# list of items to test
testlst_cas <- list(
  pass_all = c("598-42-5", "19438-61-0", "20730-39-6"), # good all around
  pass_detect_only = c("1234-34-4", "0000000-01-0", "1234-12-1"), # pass detect, fail checksum
  pass_csum_only = c("598425", "19438610", "20730396") # fail detect, pass checksum
)


# Result items ------------------------------------------------------------

cas_detect_out <- list(
  pass_all = c(T, T, T),
  pass_detect_only = c(T, T, T),
  pass_csum_only = c(F, F, F)
)

cas_checkSum_out <- list(
  pass_all = c(T, T, T),
  pass_detect_only = c(F, F, F),
  pass_csum_only = c(T, T, T)
)

cas_check_out <- list(
  pass_all = c(T, T, T),
  pass_detect_only = c(F, F, F),
  pass_csum_only = c(F, F, F)
)


# Tests -------------------------------------------------------------------

test_that("cas_detect behaves as expected with typical input", {
  tst <- Map(cas_detect, testlst_cas)
  expect_identical(tst, cas_detect_out)
})

test_that("cas_checkSum behaves as expected with typical input", {
  tst <- Map(cas_checkSum, testlst_cas)
  expect_identical(tst, cas_checkSum_out)
})

test_that("cas_check behaves as expected with typical input", {
  #tst <- Map(cas_check, testlst_cas)
  expect_warning(Map(cas_check, testlst_cas), "No validly formatted inputs detected")
  #expect_identical(tst, cas_check_out)
})

