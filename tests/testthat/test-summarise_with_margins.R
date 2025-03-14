# nolint start: line_length_linter
# We can use dbplyr::memdb_frame(roster) to create an in memory lazy table using
# sqlite driver, but we use duckdb.
# Three main reasons to use duckdb instead of sqlite:
# * duckdb also supports in memory database.
# * R4DS (2e) also uses duckdb in Databases section.
#   https://r4ds.hadley.nz/databases#in-this-book
# * We use like `paste(x, collapse = "/")` in testthat,
#   but need to replace it with `stringr::str_flatten(x, collapse = "/")`
#   in the context of dbplyr in lazy table.
#   str_flatten() is not supported in sqlite, but it is supported in duckdb.
# The disadvantage of using duckdb is that the order of the rows in the result
# of aggregate functions is different each time.
# https://duckdb.org/docs/sql/aggregates.html#order-by-clause-in-aggregate-functions
# Therefore, when testing a lazy table, check whether the data are equal,
# ignoring the order of the rows.
# nolint end
test_that(".margin works correctly with local data frame and lazy table", {
  run_test <- function(lazy) {
    roster <- get_roster_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "roster", roster)

      roster <- dplyr::tbl(con, "roster")
    }

    actual <- summarise_with_margins(
      roster,
      n = dplyr::n(),
      mean = mean(score, na.rm = TRUE),
      .margins = c(program, delin, level)
    )

    expected <- list(
      roster %>%
        dplyr::summarise(
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          program = "(all)",
          delin = "(all)",
          level = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = program,
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          delin = "(all)",
          level = "(all)",
        ),
      roster %>%
        dplyr::summarise(
          .by = c(program, delin),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          level = "(all)",
        ),
      roster %>%
        dplyr::summarise(
          .by = c(program, delin, level),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(program, delin, level) |>
      dplyr::arrange(program, delin, level)

    if (lazy) {
      # If lazy = TRUE, test the original actual and expected are equal ignoring row order
      actual <- arrange_then_collect(actual)
      expected <- arrange_then_collect(expected)
    } else {

    }

    expect_identical(actual, expected)
  }

  purrr::walk(
    c(FALSE, TRUE),
    run_test
  )
})

test_that(".without_all works correctly with local data frame and lazy table", {
  run_test <- function(lazy) {
    roster <- get_roster_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "roster", roster)

      roster <- dplyr::tbl(con, "roster")
    }

    actual <- summarise_with_margins(
      roster,
      n = dplyr::n(),
      mean = mean(score, na.rm = TRUE),
      .margins = c(program, delin, level),
      .without_all = year
    )

    expected <- list(
      roster %>%
        dplyr::summarise(
          .by = year,
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          program = "(all)",
          delin = "(all)",
          level = "(all)",
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          delin = "(all)",
          level = "(all)",
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          level = "(all)",
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin, level),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE)
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(year, program, delin, level) |>
      dplyr::arrange(year, program, delin, level)

    if (!lazy) {
      expected <- dplyr::arrange(expected, year)
    }

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

test_that(".with_all works correctly with local data frame and lazy table", {
  run_test <- function(lazy) {
    roster <- get_roster_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "roster", roster)

      roster <- dplyr::tbl(con, "roster")
    }

    actual <- summarise_with_margins(
      roster,
      n = dplyr::n(),
      mean = mean(score, na.rm = TRUE),
      .margins = c(program, delin, level),
      .without_all = year,
      .with_all = c(site, school_year)
    )

    expected <- list(
      # all program, delin, level
      roster %>%
        dplyr::summarise(
          .by = year,
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          program = "(all)",
          delin = "(all)",
          level = "(all)",
          site = "(all)",
          school_year = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, school_year),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          program = "(all)",
          delin = "(all)",
          level = "(all)",
          site = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, site),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          program = "(all)",
          delin = "(all)",
          level = "(all)",
          school_year = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, site, school_year),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          program = "(all)",
          delin = "(all)",
          level = "(all)"
        ),
      # by program, all delin, level
      roster %>%
        dplyr::summarise(
          .by = c(year, program),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          delin = "(all)",
          level = "(all)",
          site = "(all)",
          school_year = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, school_year),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          delin = "(all)",
          level = "(all)",
          site = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, site),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          delin = "(all)",
          level = "(all)",
          school_year = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, site, school_year),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          delin = "(all)",
          level = "(all)"
        ),
      # by program, delin, all level
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          level = "(all)",
          site = "(all)",
          school_year = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin, school_year),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          level = "(all)",
          site = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin, site),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          level = "(all)",
          school_year = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin, site, school_year),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          level = "(all)"
        ),
      # by program, delin, level
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin, level),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          site = "(all)",
          school_year = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin, level, school_year),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          site = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin, level, site),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          school_year = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, program, delin, level, site, school_year),
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE)
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(year, program, delin, level, site, school_year) |>
      dplyr::arrange(year, program, delin, level, site, school_year)

    if (!lazy) {
      expected <- dplyr::arrange(expected, year)
    }

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

test_that(".margin_name works correctly with local data frame and lazy table", {
  run_test <- function(lazy, margin_var, margine_name) {
    roster <- get_roster_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "roster", roster)

      roster <- dplyr::tbl(con, "roster")
    }

    actual <- summarise_with_margins(
      roster,
      n = dplyr::n(),
      mean = mean(score, na.rm = TRUE),
      .margins = {{ margin_var }},
      .margin_name = margine_name
    )

    expected <- list(
      roster %>%
        dplyr::summarise(
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE),
          "{{ margin_var }}" := margine_name
        ),
      roster %>%
        dplyr::summarise(
          .by = {{ margin_var }},
          n = dplyr::n(),
          mean = mean(score, na.rm = TRUE)
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate({{ margin_var }}) |>
      dplyr::arrange(dplyr::pick({{ margin_var }}))

    if (lazy) {
      actual <- arrange_then_collect(actual)
      expected <- arrange_then_collect(expected)
    }

    expect_identical(actual, expected)
  }

  # run tests for lazy and not lazy data at once
  run_test_both <- function(margin_var, margine_name) {
    purrr::walk(
      c(FALSE, TRUE),
      function(lazy) run_test(lazy, {{ margin_var }}, margine_name)
    )
  }

  # basic case
  run_test_both(level, "total")

  # since "ES250" is used in level, it can not be used as margin name
  expect_error(
    run_test_both(level, "ES250"),
    "not allowed as a margin name"
  )

  # since NA_character_ is used in level, it can not be used as margin name
  expect_error(
    run_test_both(level, NA_character_),
    "not allowed as a margin name"
  )

  # delin has no NA_character_, so no error
  run_test_both(delin, NA_character_)
})

test_that("Can calculate something using the margin variable before calculating the margin with local data frame and lazy table", {
  run_test <- function(lazy, ...) {
    roster <- get_roster_dummy() %>%
      # for easier debugging, extract the student taking more than one course
      dplyr::filter(student_id == "1000108")

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "roster", roster)

      roster <- dplyr::tbl(con, "roster")
    }

    actual <- summarise_with_margins(
      roster,
      ...,
      .margins = c(delin, level),
      .without_all = year,
      .with_all = site
    )

    expected <- list(
      # all delin, level
      roster %>%
        dplyr::summarise(
          .by = year,
          ...,
          delin = "(all)",
          level = "(all)",
          site = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, site),
          ...,
          delin = "(all)",
          level = "(all)"
        ),
      # all level
      roster %>%
        dplyr::summarise(
          .by = c(year, delin),
          ...,
          level = "(all)",
          site = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, delin, site),
          ...,
          level = "(all)"
        ),
      # by delin, level
      roster %>%
        dplyr::summarise(
          .by = c(year, delin, level),
          ...,
          site = "(all)"
        ),
      roster %>%
        dplyr::summarise(
          .by = c(year, delin, level, site),
          ...
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(year, delin, level, site) |>
      dplyr::arrange(year, delin, level, site)

    if (!lazy) {
      expected <- dplyr::arrange(expected, year)
    }

    if (lazy) {
      actual <- arrange_then_collect(actual)
      expected <- arrange_then_collect(expected)
    }

    expect_identical(actual, expected)
  }

  purrr::walk(
    c(FALSE, TRUE),
    function(lazy) run_test(lazy, n = dplyr::n(), levels = stringr::str_flatten(level, collapse = "/"))
  )
})
