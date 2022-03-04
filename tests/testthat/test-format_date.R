test_that("Dates are formatted", {
  expect_equal(format_date(as.Date("2022-03-04")), "March 4, 2022")
  expect_equal(format_date(as.Date("2022-11-22")), "November 22, 2022")
})
