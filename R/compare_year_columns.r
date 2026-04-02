#' Compare columns between two data frames
#'
#' Compares the columns of two data frames and reports differences in column
#' presence and type. Useful for checking consistency between datasets loaded
#' from different years or sections.
#'
#' @param df1 A data frame or tibble.
#' @param df2 A data frame or tibble.
#' @param df1_name A string label for \code{df1} used in printed output.
#'   Defaults to \code{"df1"}.
#' @param df2_name A string label for \code{df2} used in printed output.
#'   Defaults to \code{"df2"}.
#'
#' @return Invisibly returns a named list with elements:
#'   \describe{
#'     \item{only_in_df1}{Columns present only in \code{df1}.}
#'     \item{only_in_df2}{Columns present only in \code{df2}.}
#'     \item{in_both}{Columns present in both data frames.}
#'     \item{type_diffs}{Columns present in both but with differing types.}
#'   }
#'
#' @examples
#' \dontrun{
#' mort1969 <- cdc_import("mortality_multiple", 1969)
#' mort1970 <- cdc_import("mortality_multiple", 1970)
#' compare_year_columns(mort1969, mort1970, "mort1969", "mort1970")
#' }
#'
#' @export
compare_year_columns <- function(df1, df2, df1_name = "df1", df2_name = "df2") {
    cols1 <- colnames(df1)
    cols2 <- colnames(df2)

    only_in_1 <- setdiff(cols1, cols2)
    only_in_2 <- setdiff(cols2, cols1)
    in_both   <- intersect(cols1, cols2)

    cat("=== Column Comparison ===\n\n")

    cat(sprintf("Total columns in %s: %d\n", df1_name, length(cols1)))
    cat(sprintf("Total columns in %s: %d\n\n", df2_name, length(cols2)))

    if (length(only_in_1) == 0) {
        cat(sprintf("Columns only in %s: none\n\n", df1_name))
    } else {
        cat(sprintf("Columns only in %s (%d):\n", df1_name, length(only_in_1)))
        cat(paste0("  - ", only_in_1, collapse = "\n"), "\n\n")
    }

    if (length(only_in_2) == 0) {
        cat(sprintf("Columns only in %s: none\n\n", df2_name))
    } else {
        cat(sprintf("Columns only in %s (%d):\n", df2_name, length(only_in_2)))
        cat(paste0("  - ", only_in_2, collapse = "\n"), "\n\n")
    }

    type_diffs <- Filter(Negate(is.null), lapply(in_both, function(col) {
        t1 <- class(df1[[col]])
        t2 <- class(df2[[col]])
        if (!identical(t1, t2)) {
            list(column = col, type_in_df1 = t1, type_in_df2 = t2)
        }
    }))

    if (length(type_diffs) == 0) {
        cat("Type differences in shared columns: none\n")
    } else {
        cat(sprintf("Type differences in shared columns (%d):\n", length(type_diffs)))
        for (d in type_diffs) {
            cat(sprintf("  - %s: %s in %s vs %s in %s\n",
                        d$column, d$type_in_df1, df1_name, d$type_in_df2, df2_name))
        }
    }

    invisible(list(
        only_in_df1 = only_in_1,
        only_in_df2 = only_in_2,
        in_both     = in_both,
        type_diffs  = type_diffs
    ))
}