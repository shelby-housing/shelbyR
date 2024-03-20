#' Read raw Lead + Rehab data into R.
#'
#' @param file See shelbyR_raw_paths for full list of files
#'
#' @import readxl
#' @importFrom janitor clean_names
#'
#' @return df of raw data
#' @export
shelbyR_read_dataraw <- function(file) {
  fp <- shelbyR_get_path(file)

  if (str_ends(fp, "xlsx")) {

    sheet <- match(file, shelbyR_raw_paths$file)
    sheet <- shelbyR_raw_paths$sheet[sheet]

    skip <- match(file, shelbyR_raw_paths$file)
    skip <- shelbyR_raw_paths$skip[skip]

    df <- read_excel(fp, sheet = sheet, skip = skip)
  }

  df <- df |> janitor::clean_names()
}

#' Update shelbyR cache data
#'
#' @param file Exported contractor neighborly file
#'
#' @export
#'
#' @import rappdirs dplyr
#' @importFrom readr read_csv write_csv
#' @importFrom lubridate today
shelbyR_update_cache <- function(file) {

  # get the cache
  cache_dir <- shelbyR_cache_dir()
  raw_dir <- shelbyR_cache_dir("raw")
  # file <- "contractor docs"

  raw_file <- file.path(raw_dir, paste0(gsub(" ", "-", file), ".csv"))
  df <- shelbyR_read_dataraw(file)
  raw_changes_file <- gsub(".csv", "-changes.csv", raw_file)

  # find changes to cache data
  if (file.exists(raw_file)) {
    existing_data <- read_csv(raw_file)
    data_changes <- anti_join(existing_data, df)

    # record changes to cache
    if (nrow(data_changes) > 0) {
      data_changes$file_date_change <- today()
      data_changes$file_name <- {{ file }}
      data_changes <- data_changes |> select(file_name, file_date_change, everything())
      if (file.exists(raw_changes_file)) {
        existing_changes <- read_csv(raw_changes_file)
        data_changes <- bind_rows(data_changes, existing_changes)
      }
      write_csv(data_changes, raw_changes_file)

    }
  }

  write_csv(df, raw_file)
  df
}
