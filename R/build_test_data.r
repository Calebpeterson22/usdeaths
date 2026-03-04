#' @title sample function to get a potential test dataset
#' @description Takes any dataframe and builds a small sample table
#' @param dat the dataframe to use for the sample table
#' @param nrow is the number of rows in the sample table
#' @examples
#' people <- tibble::tibble(
#'    name = c("Alice", "Bob", "Carol"),
#'    age = c(25, 30, 28),
#'    score = c(88.5, 92.0, 79.3)
#' )
#' build_test_data(people)
#' @import purrr tibble
#' @export
build_test_data <- function(dat, nrow = 50) {
  dat |>
    purrr::map(~ unique(.x)[1:nrow]) |>
    tibble::as_tibble() |>
    tidyr::fill(
      dplyr::everything(),
      .direction = "down")
}