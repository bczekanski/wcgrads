context("Test that data is correct")

test_that("data is valid", {
  expect_equal(allyrs$first[1], "DoHyun")
  expect_equal(allyrs$last[1], "Chung")
})
