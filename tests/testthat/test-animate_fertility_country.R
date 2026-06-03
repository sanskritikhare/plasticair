test_that("animate_fertility_country works", {
  dat <- tibble::tibble(
    year = c(2000, 2001, 2002),
    fertility_rate = c(3.5, 3.4, 3.3)
  )

  result <- animate_fertility_country(dat)

  expect_s3_class(result, "gif_image")
})

test_that("validate_fertility_data works", {
  test_data <- data.frame(
    year = c(2020, 2021),
    fertility_rate = c(2.5, 2.4)
  )

  expect_equal(validate_fertility_data(test_data), test_data)
  expect_error(validate_fertility_data("not data"))
  expect_error(validate_fertility_data(data.frame(year = 2020)))
})
