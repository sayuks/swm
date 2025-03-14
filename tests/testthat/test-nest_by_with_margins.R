test_that("nest_by_with_margins() works correctly with local data", {
  roster <- get_roster_dummy()

  actual <- nest_by_with_margins(
    roster,
    .margins = c(delin, level),
    .without_all = year,
    .with_all = site,
    .key = "nested",
    .keep = TRUE
  )

  expected <- union_all_with_margins(
    roster,
    .margins = c(delin, level),
    .without_all = year,
    .with_all = site
  ) %>%
    dplyr::nest_by(
      year, delin, level, site,
      .key = "nested",
      .keep = TRUE
    ) %>%
    dplyr::arrange(year, delin, level, site)

  expect_identical(actual, expected)
})
