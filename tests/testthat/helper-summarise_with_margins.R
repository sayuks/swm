# create dummy roster data for using in testthat context
get_roster_dummy <- function(factor = FALSE) {
  roster <- tibble::tribble(
    ~year,       ~student_id, ~program, ~delin,      ~level,        ~site,         ~school_year,  ~score,
    2022L,       "1000101",   "Juku",   "Core",      "ES110",       "Jiyugaoka",   "ES6",         NA_integer_,
    2022L,       "1000102",   "Juku",   "Core",      "ES110",       "Shibuya",     NA_character_, 69L,
    2022L,       "1000103",   "Juku",   "Core",      NA_character_, "Yotsuya",     "ES5",         60L,
    2022L,       "1000104",   "Juku",   "Core",      "ES150",       "Jiyugaoka",   "JHS1",        69L,
    2022L,       "1000105",   "Juku",   "Core",      "ES250",       "Jiyugaoka",   NA_character_, 59L,
    2022L,       "1000106",   "Juku",   "Core",      "ES250",       "Jiyugaoka",   "JHS2",        93L,
    2022L,       "1000107",   "Juku",   "Domestic",  "EA580",       "Shibuya",     "SHS1",        55L,
    2022L,       "1000108",   "Juku",   "Domestic",  "MA690",       "Shibuya",     "SHS3",        76L,
    2022L,       "1000108",   "Juku",   "Domestic",  "EA690",       "Yotsuya",     "SHS3",        NA_integer_,
    2022L,       "1000105",   "Juku",   "Domestic",  "MS250",       "Yokohama",    "JHS2",        80L,
    2022L,       "1000108",   "Juku",   "Scholars",  "LA510",       "Jiyugaoka",   "SHS3",        91L,
    2022L,       "1000109",   "Juku",   "Scholars",  "SA500",       "Shibuya",     "JHS3",        72L,
    2022L,       "1000201",   "Kids",   "Kids-ES",   "ES015",       "Jiyugaoka",   "ES4",         NA_integer_,
    2022L,       "1000202",   "Kids",   "Kids-ES",   "ES015",       "Kichijoji",   "ES3",         74L,
    2022L,       "1000203",   "Kids",   "Kids-ES",   "ES040",       NA_character_, "ES3",         58L,
    2022L,       "1000204",   "Kids",   "Kids-ES",   "ES040",       "Kichijoji",   "ES4",         86L,
    2022L,       "1000205",   "Kids",   "Explorers", "EX100",       "Yotsuya",     "ES1",         80L,
    2022L,       "1000204",   "Kids",   "Explorers", "EX250",       "Jiyugaoka",   "ES4",         75L,
    2023L,       "1000101",   "Juku",   "Core",      "ES110",       "Jiyugaoka",   "ES6",         80L,
    2023L,       "1000102",   "Juku",   "Core",      "ES110",       "Shibuya",     NA_character_, 52L,
    2023L,       "1000103",   "Juku",   "Core",      NA_character_, "Yotsuya",     "ES5",         33L,
    2023L,       "1000104",   "Juku",   "Core",      "ES150",       "Jiyugaoka",   "JHS1",        49L,
    2023L,       "1000105",   "Juku",   "Core",      "ES250",       "Jiyugaoka",   NA_character_, 72L,
    2023L,       "1000106",   "Juku",   "Core",      "ES250",       "Jiyugaoka",   "JHS2",        68L,
    2023L,       "1000107",   "Juku",   "Domestic",  "EA580",       "Shibuya",     "SHS1",        59L,
    2023L,       "1000108",   "Juku",   "Domestic",  "MA690",       "Shibuya",     "SHS3",        NA_integer_,
    2023L,       "1000108",   "Juku",   "Domestic",  "EA690",       "Yotsuya",     "SHS3",        31L,
    2023L,       "1000105",   "Juku",   "Domestic",  "MS250",       "Yokohama",    "JHS2",        37L,
    2023L,       "1000108",   "Juku",   "Scholars",  "LA510",       "Jiyugaoka",   "SHS3",        84L,
    2023L,       "1000109",   "Juku",   "Scholars",  "SA500",       "Shibuya",     "JHS3",        95L,
    2023L,       "1000201",   "Kids",   "Kids-ES",   "ES015",       "Jiyugaoka",   "ES4",         78L,
    2023L,       "1000202",   "Kids",   "Kids-ES",   "ES015",       "Kichijoji",   "ES3",         31L,
    2023L,       "1000203",   "Kids",   "Kids-ES",   "ES040",       NA_character_, "ES3",         63L,
    2023L,       "1000204",   "Kids",   "Kids-ES",   "ES040",       "Kichijoji",   "ES4",         82L,
    2023L,       "1000205",   "Kids",   "Explorers", "EX100",       "Yotsuya",     "ES1",         68L,
    2023L,       "1000204",   "Kids",   "Explorers", "EX250",       "Jiyugaoka",   "ES4",         61L,
    NA_integer_, "1000101",   "Juku",   "Core",      "ES110",       "Jiyugaoka",   "ES6",         100L,
  )

  if (factor) {

    roster <- roster |>
      dplyr::mutate(
        # Don't use `jprep.datasets::fct_*()` families to reduce extra dependency.
        # program does not have NAs in its values, but can contain NA in the level
        program = factor(program, c("Kids", "Juku", "A+", "Sunnyside", NA), exclude = NULL),
        # delin has no NAs, but ordered in this example.
        delin = factor(delin, c("Kids-ES", "Explorers", "Core", "Domestic", "Scholars", "A+", "Sunnyside"), ordered = TRUE),
        # add level "(all)" to the last
        site = factor(site, c("Jiyugaoka", "Shibuya", "Kichijoji", "Yotsuya", "Yokohama", "Online", "(all)")),
        # include NA in the level (school_year has NAs in its values)
        # school_year is a ordered factor
        school_year = factor(school_year,
                             c("ES1", "ES2", "ES3", "ES4", "ES5", "ES6",
                             "JHS1", "JHS2", "JHS3", "SHS1", "SHS2", "SHS3", NA),
                             exclude = NULL,
                             ordered = TRUE),
      )
  }

  roster
}

# for easier debugging for lazy tables
#
# There is also a way to test lazy tables without collecting and reorder rows,
# but to make it easier to understand the difference between specific rows if test is failed,
# reorder rows (using all columns) and then collect.
# If two data to which this function is applied are equal,
# it means that the two data before this function is applied are equal, ignoring row order.
arrange_then_collect <- function(data) {
  data <- dplyr::arrange(data, dplyr::pick(tidyselect::everything()))
  dplyr::collect(data)
}
