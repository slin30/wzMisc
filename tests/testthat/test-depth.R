context("depth")

## TODO: Rename context
## TODO: Add more tests

tst_lsts <- list(
  list(1:3),
  list(list(1:3)),
  list(1:3, "a", list(1:2)),
  list(1:3, "a", list(list(1:2))),
  list(1:3, "a", list(list(1:2), list(1:10, list(LETTERS[1:3])))),
  list(1:3, "a", list(list(1:2), list(1:10, list(list(LETTERS[1:3]))))),
  list(1:3, "a", list(list(1:2), list(1:10, list(list(list(LETTERS[1:3]))))))
)

test_that("depth and depth_while are identical", {
  res_depth       <- Map(depth, tst_lsts)
  res_depth_while <- Map(depth_while, tst_lsts)
  expect_true(identical(res_depth, res_depth_while))
})
