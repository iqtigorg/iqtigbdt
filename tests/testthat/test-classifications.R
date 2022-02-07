context("classification-functions")

#### compute_rate_dc

test_that("Testing arguments value/warn_if_n0", {
  expect_warning(res <- compute_rate_dc(0, 0, 0),
    regexp = "n = 0"
  )
  expect_true(is.na(res))
  expect_silent(res <- compute_rate_dc(0, 0, 0.5, warn_if_n0 = FALSE))
  expect_true(is.na(res))
  expect_silent(res <- compute_rate_dc(0, 0, 0.5,
    alternative = "less",
    warn_if_n0 = FALSE
  ))
  expect_true(is.na(res))
  expect_silent(res <- compute_rate_dc(0, 0:10, 0.5,
    value_if_n0 = 7,
    warn_if_n0 = FALSE
  ))
  expect_equal(res, c(7, compute_rate_dc(0, 1:10, 0.5)))
})

test_that("Empty input gives emtpy output", {
  expect_equal(
    compute_rate_dc(numeric(), 0, 0.5),
    numeric()
  )
  expect_equal(
    compute_rate_dc(numeric(), 0, 0.5),
    numeric()
  )
  expect_equal(
    compute_rate_dc(numeric(), 0, 0.5),
    numeric()
  )
})

test_that("Input handling of compute_rate_dc", {
  expect_error(compute_rate_dc(c("1", "2"), 40, 0.5),
    regexp = "o.*not.*numeric"
  )
  expect_error(compute_rate_dc(list(1, 2), 40, 0.5),
    regexp = "o.*not.*numeric"
  )
  expect_error(compute_rate_dc(c(0.5, 1:30), 40, 0.5),
    regexp = "o.*not.*integ"
  )
  expect_error(compute_rate_dc(10:-1, 40, 0.5),
    regexp = "o.*small.*0"
  )
  expect_error(
    suppressWarnings(compute_rate_dc(1:10, 10:4, 0.5)),
    regexp = "o.*larg.*n"
  )
  expect_error(compute_rate_dc(0, c("1", "2"), 0.5),
    regexp = "n.*not.*numeric"
  )
  expect_error(compute_rate_dc(0, list(1, 2), 0.5),
    regexp = "n.*not.*numeric"
  )
  expect_error(compute_rate_dc(0, c(0.5, 1:30), 0.5),
    regexp = "n.*not.*integ"
  )
  expect_error(compute_rate_dc(2, 5, c("0.1", "0.2")),
    regexp = "t.*not.*numeric"
  )
  expect_error(compute_rate_dc(2, 5, list(0.1, 0.2)),
    regexp = "t.*not.*numeric"
  )
  expect_error(compute_rate_dc(2, 5, c(0.2, -0.1, 0.3)),
    regexp = "t.*small.*0"
  )
  expect_error(compute_rate_dc(2, 5, 0.3, alternative = "asdf"),
    regexp = "alternative*"
  )
  expect_error(compute_rate_dc(2, 5, 0.3, value_if_n0 = "yes"),
    regexp = "value_if_n0.*not.*numeric"
  )
  expect_error(compute_rate_dc(2, 5, 0.3, value_if_n0 = c(1, 2)),
    regexp = "value_if_n0.*not.*length"
  )
  expect_error(compute_rate_dc(2, 5, 0.3, warn_if_n0 = 0),
    regexp = "warn_if_n0.*not.*logic"
  )
  expect_error(compute_rate_dc(2, 5, 0.3, warn_if_n0 = c(TRUE, TRUE)),
    regexp = "warn_if_n0.*not.*length"
  )
})

test_that("Input handling of compute_oe_dc", {
  expect_error(compute_oe_dc(c("1", "2"), 40, 0.5),
               regexp = "o.*not.*numeric"
  )
  expect_error(compute_oe_dc(list(1, 2), 40, 0.5),
               regexp = "o.*not.*numeric"
  )
  expect_error(compute_oe_dc(c(0.5, 1:30), 40, 0.5),
               regexp = "o.*not.*integ"
  )
  expect_error(compute_oe_dc(10:-1, 40, 0.5),
               regexp = "o.*small.*0"
  )
  expect_error(compute_oe_dc(0, c("1", "2"), 0.5),
               regexp = "e.*not.*numeric"
  )
  expect_error(compute_oe_dc(0, list(1, 2), 0.5),
               regexp = "e.*not.*numeric"
  )
  expect_error(compute_oe_dc(2, 5, c("0.1", "0.2")),
               regexp = "t.*not.*numeric"
  )
  expect_error(compute_oe_dc(2, 5, list(0.1, 0.2)),
               regexp = "t.*not.*numeric"
  )
  expect_error(compute_oe_dc(2, 5, c(0.2, -0.1, 0.3)),
               regexp = "t.*small.*0"
  )
  expect_error(compute_oe_dc(2, 5, 0.3, alternative = "asdf"),
               regexp = "alternative*"
  )
  expect_error(compute_oe_dc(2, 5, 0.3, value_if_e0 = "yes"),
               regexp = "value_if_e0.*not.*numeric"
  )
  expect_error(compute_oe_dc(2, 5, 0.3, value_if_e0 = c(1, 2)),
               regexp = "value_if_e0.*not.*length"
  )
  expect_error(compute_oe_dc(2, 5, 0.3, warn_if_e0 = 0),
               regexp = "warn_if_e0.*not.*logic"
  )
  expect_error(compute_oe_dc(2, 5, 0.3, warn_if_e0 = c(TRUE, TRUE)),
               regexp = "warn_if_e0.*not.*length"
  )
})

### classify_dc

test_that("Input handling of classify_dc", {

expect_error(classify_dc(2, 5, .02, threshold_value = "0.05"),
             regexp = "threshold_value.*not.*numeric"
)
expect_error(classify_dc(2, 5, .2, threshold_value = -0.05),
             regexp = "threshold_value.*small.*0"
)

})
