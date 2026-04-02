test_that("compare_year_columns detects no differences between identical data frames", {
  df1 <- data.frame(a = 1, b = 2, c = 3)
  df2 <- data.frame(a = 1, b = 2, c = 3)

  result <- compare_year_columns(df1, df2)

  expect_equal(result$only_in_df1, character(0))
  expect_equal(result$only_in_df2, character(0))
  expect_equal(length(result$type_diffs), 0)
})

test_that("compare_year_columns detects columns only in df1", {
  df1 <- data.frame(a = 1, b = 2, c = 3)
  df2 <- data.frame(a = 1, b = 2)

  result <- compare_year_columns(df1, df2)

  expect_equal(result$only_in_df1, "c")
  expect_equal(result$only_in_df2, character(0))
})

test_that("compare_year_columns detects columns only in df2", {
  df1 <- data.frame(a = 1, b = 2)
  df2 <- data.frame(a = 1, b = 2, d = 4)

  result <- compare_year_columns(df1, df2)

  expect_equal(result$only_in_df1, character(0))
  expect_equal(result$only_in_df2, "d")
})

test_that("compare_year_columns detects type differences in shared columns", {
  df1 <- data.frame(a = 1, b = 2)
  df2 <- data.frame(a = 1, b = "hello")

  result <- compare_year_columns(df1, df2)

  expect_equal(length(result$type_diffs), 1)
  expect_equal(result$type_diffs[[1]]$column, "b")
  expect_equal(result$type_diffs[[1]]$type_in_df1, "numeric")
  expect_equal(result$type_diffs[[1]]$type_in_df2, "character")
})

test_that("compare_year_columns handles all differences at once", {
  df1 <- data.frame(a = 1, b = 2, c = 3)
  df2 <- data.frame(a = 1, b = "hello", d = 4)

  result <- compare_year_columns(df1, df2)

  expect_equal(result$only_in_df1, "c")
  expect_equal(result$only_in_df2, "d")
  expect_equal(length(result$type_diffs), 1)
  expect_equal(result$type_diffs[[1]]$column, "b")
})

test_that("compare_year_columns returns invisibly", {
  df1 <- data.frame(a = 1)
  df2 <- data.frame(a = 1)

  expect_invisible(compare_year_columns(df1, df2))
})