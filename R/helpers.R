#' Install the filepath of your rehab folder
#'
#' @param filepath the filepath to your outlook lead + rehab folder
#' @export
#'
#' @examples
#' \dontrun{
#' shelbyR_set_path("path/to/folder")
#' # Reload your environment to use the key without restarting session
#' readRenviron("~/.Renviron")
#' # Check the install with:
#' Sys.getenv("LEAD_REHAB_DIR")
#' }
shelbyR_set_path <- function(filepath) {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if (!file.exists(renv)) {
    file.create(renv)
  }

  Sys.setenv(LEAD_REHAB_DIR = filepath)
  message('Your folderpath has been stored in your .Renviron. \nAccess it with Sys.getenv("LEAD_REHAB_DIR"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
}

shelbyR_raw_paths <- tibble(
  file = c(
    "rehab cases",
    "queue",
    "older cases",
    "cancelled",
    "recert",
    "bids",
    "contractor docs",
    "neighborly rehab",
    "neighborly lead",
    "neighborly logs",
    "contractor names"
  ),
  base = c(
    "Rehab Cases by Program Year.xlsx",
    "Rehab Cases by Program Year.xlsx",
    "Rehab Cases by Program Year.xlsx",
    "Rehab Cases by Program Year.xlsx",
    "Rehab Cases by Program Year.xlsx",
    "Bid Days",
    "archive",
    "archive",
    "archive",
    "archive",
    paste0("contractors", .Platform$file.sep, "contractor-docs.xlsx")
  ),
  pattern = c(
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "Contractors_.*.xlsx",
    "DataExport_Home.*.xlsx",
    "DataExport_Lead.*.xlsx",
    "DataExport_Home.*.xlsx",
    NA_character_
  ),
  sheet = c(
    "PY23",
    "Queue",
    "PY21-22",
    "Cancelled",
    "Recert",
    NA_character_,
    "Contractors",
    "Cases",
    "Cases",
    "Case Audit Logs",
    "contractor-docs"
  ),
  skip = c(
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
  )
)

#' Get the filepath for a rehab file/folder
#'
#' @param file The file or folder to get the filepath for.
#'
#' @return A character string
#' @export
#'
#' @examples
#' # Get the bid folder
#' shelbyR_get_path("bids")
shelbyR_get_path <- function(file = NULL) {
  fp <- Sys.getenv("LEAD_REHAB_DIR")

  if (nchar(fp) == 0) {
    stop('No OneDrive folderpath set. Use shelbyR_set_path() to set the path to your `OneDrive Lead + Rehab` folder.')
  }

  if (!is.null(file)) {
    base <- match(file, shelbyR_raw_paths$file)
    base <- shelbyR_raw_paths$base[base]

    fp <- file.path(fp, base)

    pattern <- match(file, shelbyR_raw_paths$file)
    pattern <- shelbyR_raw_paths$pattern[pattern]

    if (!is.na(pattern)) {
      fp <- list.files(fp, pattern, full.names = TRUE)
    }
  }
  fp
}

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

#' Get cache directory
#'
#' @param folder cache folder to return (and create, if needed)
#'
#' @return cache_dir: folder path of cache directory
#' @export
#'
#' @import rappdirs
shelbyR_cache_dir <- function(folder = NULL) {

  cache_dir <- rappdirs::user_cache_dir("shelbyR")

  # create base cache directory if it does not exist
  if (!file.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message(paste("Created shelbyR cache at: ", cache_dir))
  }

  # get specific cache directory, create if needed.
  if (!is.null(folder)) {
    cache_dir <- file.path(cache_dir, {{ folder }})

    if (!file.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
      message(paste("Created cache dir: ", cache_dir))
    }
  } else {
    cache_dir
  }

  # return cache directory filepath
  cache_dir
}

#' Update shelbyR cache data
#'
#' @param file Exported contractor neighborly file
#'
#' @export
#'
#' @import rappdirs
#' @importFrom readr read_csv
shelbyR_update_cache <- function(file) {

  # get the cache
  cache_dir <- shelbyR_cache_dir()
  raw_dir <- shelbyR_cache_dir("raw")
  # file <- "contractor docs"

  raw_file <- file.path(raw_dir, paste0(gsub(" ", "-", file), ".csv"))

  if (!file.exists(raw_file)) {
    rehab_file <- shelbyR_get_path(file)
  }

}

#' Clean a string to Title Case & string squish
#'
#' @param x A string to clean
#' @param case "lower", "upper", "title", or "title up" for title case with three letter segments in uppercase (useful for many abbreviations).
#' @param punct Keep punctuation?
#' @param spaces Keep spaces?
#'
#' @return A Title Case String.
#'
#' @import stringr
#' @export
clean_string <- function(x, case = FALSE, punct = TRUE, spaces = TRUE) {

  if (punct == FALSE) {
    x <- str_replace_all(x, "[:punct:]|[:symbol:]", " ")
  }

  if (case == "title") {
    x <- str_to_title(x)
  } else if (case == "title up") {
    x <- str_to_title(x)
    x <- str_replace_all(x, "\\b[\\w[:punct:]]{3}\\b", toupper)
  } else if (case == "lower") {
    x <- str_to_lower(x)
  } else if (case == "upper") {
    x <- str_to_upper(x)
  } else {
    x
  }

  if (spaces == FALSE) {
    x <- str_remove_all(x, "\\s")
  }

  x <- str_squish(x)
}

#' Clean columns and remove empty columns
#'
#' @param df A data frame to clean
#' @param sort Sort the columns by name
#' @param all_character Change all columns to character type?
#'
#' @return A cleaned dataframe.
#' @export
#'
#' @import dplyr
#' @importFrom janitor clean_names remove_empty
clean_columns <- function(df, sort = FALSE, all_character = FALSE) {
  df <- df %>% clean_names() %>% remove_empty("cols")

  if (sort == TRUE) {
    df <- df %>% select(sort(names(.)))
  }

  if (all_character == TRUE) {
    df %>% mutate(across(everything(), ~ as.character(.x)))
  }

  df
}
