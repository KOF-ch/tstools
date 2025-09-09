test_that("wide_to_ts", {
  df <- data.frame(
    date = c("2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4"),
    gdp = c(100, 105, 110, 120),
    cpi = c(2.1, 2.2, 2.3, 2.5)
  )

  out <- wide_to_ts(df)

  expected <- list(
    gdp = structure(c(100, 105, 110, 120), tsp = c(2024, 2024.75, 4), class = "ts"),
    cpi = structure(c(2.1, 2.2, 2.3, 2.5), tsp = c(2024, 2024.75, 4), class = "ts")
  )

  expect_equal(out, expected)
})
