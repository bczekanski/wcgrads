context("Test that data is correct")

test_that("data is valid", {
  data("all")
  expect_equal(allyrs$first[1], "DoHyun")
})
