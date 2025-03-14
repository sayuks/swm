test_that("nest_with_margins() works correctly with local data", {
  roster <- get_roster_dummy()

  actual <- nest_with_margins(
    roster,
    .margins = c(delin, level),
    .without_all = year,
    .with_all = site,
    .key = "nested"
  )

  expected <- union_all_with_margins(
    roster,
    .margins = c(delin, level),
    .without_all = year,
    .with_all = site
  ) %>%
    tidyr::nest(
      .by = c(year, delin, level, site),
      .key = "nested"
    ) %>%
    dplyr::arrange(year, delin, level, site)

  expect_identical(actual, expected)
})
