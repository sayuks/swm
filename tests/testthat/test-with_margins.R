test_that("get_all_subsets() works correctly", {
  x <- c("A", "B", "C")

  expect_identical(
    get_all_subsets(x),
    list(
      character(0),
      "A",
      "B",
      "C",
      c("A", "B"),
      c("A", "C"),
      c("B", "C"),
      c("A", "B", "C")
    )
  )

  expect_identical(
    get_all_subsets(x, rev = TRUE),
    list(
      character(0),
      "C",
      "B",
      "A",
      c("B", "C"),
      c("A", "C"),
      c("A", "B"),
      c("A", "B", "C")
    )
  )
})

test_that("get_hierarchy() works correctly", {
  x <- c("A", "B", "C")

  expect_identical(
    get_hierarchy(x),
    list(
      character(0),
      "A",
      c("A", "B"),
      c("A", "B", "C")
    )
  )
})

test_that("assert_column_intersect() works correctly", {
  l <- list(
    g1 = "A",
    g2 = c("A", "B"),
    g3 = c("A", "B", "D")
  )

  expect_error(
    assert_column_intersect(l),
    "common columns"
  )
})

test_that("assert_margin_name() works correctly with local data frame and lazy table", {
  run_test <- function(lazy) {
    d <- data.frame(
      x = c(NA_character_, "a"),
      y = c("b", NA_character_),
      z = c("a", "b")
    )

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "d", d)

      d <- dplyr::tbl(con, "d")
    }

    # basic success
    expect_no_error(
      assert_margin_name(d, "all")
    )

    # basic error
    expect_error(
      assert_margin_name(d, "a"),
      "not allowed as a margin name"
    )

    # NA_character_ can be used as margin_name
    expect_no_error(
      assert_margin_name(dplyr::select(d, z), NA_character_)
    )

    # NA_character_ is also error-checked
    expect_error(
      assert_margin_name(d, NA_character_),
      "not allowed as a margin name"
    )
  }

  purrr::walk(
    c(FALSE, TRUE),
    run_test
  )
})

test_that("with_margins() can also not create margins", {
  run_test <- function(lazy) {
    roster <- get_roster_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "roster", roster)

      roster <- dplyr::tbl(con, "roster")
    }

    # return as is
    .f <- function(.data, ..., .margin_pairs, .by) {
      .data
    }

    actual <- with_margins(
      roster,
      .margins = c(program, delin, level),
      .f = .f,
      .sort = FALSE
    )

    expected <- list(
      roster,
      roster,
      roster,
      roster
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(c(program, delin, level))

    if (lazy) {
      actual <- dplyr::collect(actual)
      expected <-  dplyr::collect(expected)
    }

    expect_equal(actual, expected)
  }

  purrr::walk(
    c(FALSE, TRUE),
    run_test
  )
})

test_that("with_margins() can reconstruct factors as expexted in a local data frame", {
    roster <- get_roster_dummy(factor = TRUE)

    # just make margins
    .f <- function(.data, ..., .margin_pairs, .by) {
      dplyr::mutate(.data, !!!.margin_pairs)
    }

    # Case 1: .margin_name is not a NA_character_ ----
    res1 <- with_margins(
      roster,
      .margins = c(program, delin, level),
      .without_all = year,
      .with_all = c(site, school_year),
      .f = .f,
      .margin_name = "(all)",
      .sort = TRUE
    )

    # factor levels as expected (including NA in levels)
    expect_identical(
      res1 %>%
        dplyr::select(tidyselect::where(is.factor)) %>%
        lapply(levels),
      list(
        # .margin_name "(all)" comes at the beginning of level.
        # If .margin_name is not NA, it causes no error
        # even if the levels contain NA.
        program = c("(all)", "Kids", "Juku", "A+", "Sunnyside", NA),
        delin = c("(all)", "Kids-ES", "Explorers", "Core", "Domestic", "Scholars", "A+", "Sunnyside"),
        # originally site contained level "(all)" at the end,
        # but now comes at the beginning.
        site = c("(all)", "Jiyugaoka", "Shibuya", "Kichijoji", "Yotsuya", "Yokohama", "Online"),
        school_year = c("(all)", "ES1", "ES2", "ES3", "ES4", "ES5", "ES6",
                        "JHS1", "JHS2", "JHS3","SHS1", "SHS2", "SHS3", NA)
      )
    )

    # ordered or not as expected (remains unchanged)
    expect_identical(
      res1 %>%
        dplyr::select(tidyselect::where(is.factor)) %>%
        lapply(is.ordered),
      list(
        program = FALSE,
        delin = TRUE,
        site = FALSE,
        school_year = TRUE
      )
    )

    # Case 2: .margin_name is a NA_character_ ----
    # program has <NA> level but does not have values with level <NA>.
    # school_name has <NA> level and has values with level <NA>.
    # Both are detected in error.
    expect_error(
      with_margins(
        roster,
        .margins = program,
        .with_all = school_year,
        .f = .f,
        .margin_name = NA_character_,
        .sort = TRUE
      ),
      paste(
        "If `\\.margin_name` is a `NA_character_`, the following",
        "factor columns specified in `\\.margins` or `\\.with_all`",
        "must not contain <NA> in the level: `program`, `school_year`"
      )
    )

    res2 <- roster %>%
      # Since NA_character_ is used for .margin_name, the rows containing NA
      # must be deleted.
      tidyr::drop_na(delin, level, site) %>%
      with_margins(
        .margins = c(delin, level),
        .without_all = year,
        .with_all = site,
        .f = .f,
        .margin_name = NA_character_,
        .sort = TRUE
      )

    # factor levels as expected
    expect_identical(
      res2 %>%
        dplyr::select(tidyselect::where(is.factor)) %>%
        lapply(levels),
      list(
        # originally, delin and site did not include NA in the level.
        # If .margin_name is a NA_character_, level does not include NA.
        # This is consistent with the default base::factor().
        delin = c("Kids-ES", "Explorers", "Core", "Domestic", "Scholars", "A+", "Sunnyside"),
        site = c("Jiyugaoka", "Shibuya", "Kichijoji", "Yotsuya", "Yokohama", "Online", "(all)"),
        # program, school_year was not used with_margins().
        # These factors remain as they are.
        program = c("Kids", "Juku", "A+", "Sunnyside", NA),
        school_year = c(
          "ES1", "ES2", "ES3", "ES4", "ES5", "ES6", "JHS1", "JHS2", "JHS3", "SHS1",
          "SHS2", "SHS3", NA
        )
      )
    )

    # ordered or not as expected (remains unchanged)
    expect_identical(
      res2 %>%
        dplyr::select(tidyselect::where(is.factor)) %>%
        lapply(is.ordered),
      list(
        delin = TRUE,
        site = FALSE,
        program = FALSE,
        school_year = TRUE
      )
    )
})

test_that("row order is as expected when factor is specified in `.with_all` and `.margins` in local data frame", {
  x <- c(2, 10, 1, NA)

  data <- data.frame(
    x = factor(x, levels = as.character(sort(x)))
  )

  # just make margins
  f <- function(.data, ..., .margin_pairs, .by) {
    dplyr::mutate(.data, !!!.margin_pairs)
  }

  # .sort = TRUE (default)
  # sorted by x using factor levels
  actual <- swm:::with_margins(
    data,
    .with_all = x,
    .f = f,
    .sort = TRUE
  )

  expected <- data.frame(
    x = factor(
      c("(all)", "(all)", "(all)", "(all)", "1", "2", "10", NA),
      levels = c("(all)", "1", "2", "10")
    )
  )

  expect_identical(actual, expected)

  # If only one is specified in `.margins`, it is the same as the result of
  # `.with_all`.
  actual <- swm:::with_margins(
    data,
    .margins = x,
    .f = f,
    .sort = TRUE
  )

  expect_identical(actual, expected)

  # .sort = FALSE
  # With the exception of the leading (all),
  # the row order of the inputs remains the same.
  actual <- swm:::with_margins(
    data,
    .with_all = x,
    .f = f,
    .sort = FALSE
  )

  expected <- data.frame(
    x = factor(
      c("(all)", "(all)", "(all)", "(all)", "2", "10", "1", NA),
      levels = c("(all)", "1", "2", "10")
    )
  )

  expect_identical(actual, expected)
})
