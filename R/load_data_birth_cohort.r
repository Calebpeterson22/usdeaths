#' Load CDC Linked Birth/Infant Death Cohort Data
#'
#' Downloads and parses a CDC linked birth/infant death cohort zip file,
#' which contains two fixed-width files sharing the same record layout:
#' the denominator (all births) and the numerator (linked infant death records only).
#'
#' @param url Character. URL to the CDC zip file containing the cohort data.
#' @param meta A tibble of field definitions (start, end, name, type, codes)
#'   used to parse both files. Both files share the same fixed-width layout,
#'   with death-related fields (positions 194+) left blank in the denominator
#'   for surviving infants.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{denominator}{All birth records for the cohort year, including both
#'       surviving infants and those who died in infancy. Use this as the
#'       population base for rate calculations.}
#'     \item{numerator}{Linked infant death records only — a subset of the
#'       denominator where a matching death certificate was found. Use this
#'       to count infant deaths.}
#'   }
#'
#' @examples
#' url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/cohortlinkedus/LinkCO83.zip"
#' data <- load_data_birth_cohort(url, data_birth_cohort_1983)
#' decode_preview(data$denominator, data_birth_cohort_1983)
#' decode_preview(data$numerator, data_birth_cohort_1983)
#'
#' @export
load_data_birth_cohort <- function(url, meta) {
    path <- download_cdc(url)

    tempd <- tempdir()
    tempdf <- tempfile(tmpdir = tempd)
    dir.create(tempdf, showWarnings = FALSE)
    unzip(path, exdir = tempdf)

    all_files <- list.files(tempdf, full.names = TRUE)
    den_file <- all_files[grepl("den", tolower(basename(all_files)))]
    num_file <- all_files[grepl("num", tolower(basename(all_files)))]

    den <- read_section(den_file, meta)
    num <- read_section(num_file, meta)

    unlink(tempdf, recursive = TRUE)
    unlink(path)

    list(
        denominator = den,
        numerator   = num
    )
}
