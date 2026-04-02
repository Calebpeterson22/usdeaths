#' Summarize deaths by one or more grouping columns
#'
#' Groups a CDC mortality dataset by the specified column(s) and returns a
#' tibble with death counts for each group, sorted in descending order.
#'
#' @param df A decoded CDC mortality tibble, e.g. from \code{cdc_import()}.
#' @param by A character vector of column names to group by,
#'   e.g. \code{"sex"} or \code{c("sex", "race_recode3")}.
#'
#' @return A tibble with the grouping columns and a \code{n} column
#'   containing death counts, sorted descending by \code{n}.
#'
#' @examples
#' \dontrun{
#' mort1969 <- cdc_import("mortality_multiple", 1969)
#' summarize_deaths(mort1969, by = "sex")
#' summarize_deaths(mort1969, by = c("sex", "race_recode3"))
#' }
#'
#' @export
summarize_deaths <- function(df, by) {
    missing_cols <- setdiff(by, colnames(df))
    if (length(missing_cols) > 0) {
        stop(
            "The following columns were not found in the data: ",
            paste(missing_cols, collapse = ", ")
        )
    }

    df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(n))
}