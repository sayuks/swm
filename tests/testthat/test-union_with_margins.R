test_that("union_all_with_margins() works with local data and lazy tables", {
  run_test <- function(lazy) {
    roster <- get_roster_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "roster", roster)

      roster <- dplyr::tbl(con, "roster")
    }

    actual <- union_all_with_margins(
      roster,
      .margins = c(delin, level),
      .without_all = year,
      .with_all = site
    )

    expected <- list(
      # all delin, level
      roster %>%
        dplyr::mutate(
          delin = "(all)",
          level = "(all)",
          site = "(all)"
        ),
      roster %>%
        dplyr::mutate(
          delin = "(all)",
          level = "(all)"
        ),
      # all level
      roster %>%
        dplyr::mutate(
          level = "(all)",
          site = "(all)"
        ),
      roster %>%
        dplyr::mutate(
          level = "(all)"
        ),
      # by delin, level
      roster %>%
        dplyr::mutate(
          site = "(all)"
        ),
      roster
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(year, delin, level, site)


    if (lazy) {
      actual <- arrange_then_collect(actual)
      expected <- arrange_then_collect(expected)
    }

    expect_identical(actual, expected)
  }

  purrr::walk(
    c(FALSE, TRUE),
    run_test
  )
})
