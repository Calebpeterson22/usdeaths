test_that("summarize_deaths groups by a single column", {
    df <- data.frame(
        sex = c("Male", "Female", "Male", "Female", "Male"),
        age = c(65, 70, 45, 80, 55)
    )

    result <- summarize_deaths(df, by = "sex")

    expect_equal(nrow(result), 2)
    expect_true("n" %in% colnames(result))
    expect_equal(result$n[result$sex == "Male"], 3)
    expect_equal(result$n[result$sex == "Female"], 2)
})

test_that("summarize_deaths groups by multiple columns", {
    df <- data.frame(
        sex  = c("Male", "Male", "Female", "Female", "Male"),
        race = c("White", "Black", "White", "White", "White")
    )

    result <- summarize_deaths(df, by = c("sex", "race"))

    expect_equal(nrow(result), 3)
    expect_true(all(c("sex", "race", "n") %in% colnames(result)))
})

test_that("summarize_deaths sorts by descending count", {
    df <- data.frame(
        sex = c("Male", "Female", "Female", "Female", "Male")
    )

    result <- summarize_deaths(df, by = "sex")

    expect_equal(result$sex[1], "Female")
    expect_equal(result$n[1], 3)
})

test_that("summarize_deaths errors on missing columns", {
    df <- data.frame(sex = c("Male", "Female"))

    expect_error(
        summarize_deaths(df, by = "nonexistent_column"),
        "The following columns were not found in the data: nonexistent_column"
    )
})

test_that("summarize_deaths errors when some columns are missing", {
    df <- data.frame(sex = c("Male", "Female"))

    expect_error(
        summarize_deaths(df, by = c("sex", "nonexistent_column")),
        "The following columns were not found in the data: nonexistent_column"
    )
})

test_that("summarize_deaths returns a tibble", {
    df <- data.frame(sex = c("Male", "Female", "Male"))

    result <- summarize_deaths(df, by = "sex")

    expect_s3_class(result, "data.frame")
    expect_true(tibble::is_tibble(result))
})