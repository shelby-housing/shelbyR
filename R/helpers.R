#' Install the filepath of your rehab folder
#'
#' @param filepath the filepath to your rehab folder
#' @export
#'
#' @examples
#' \dontrun{
#' install_rehab_folder("path/to/folder/")
#' # Reload your environment to use the key without restarting session
#' readRenviron("~/.Renviron")
#' # Check the install with:
#' Sys.getenv("LEAD_REHAB_DIR")
#' }
install_rehab_folder <- function(filepath) {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if (!file.exists(renv)) {
    file.create(renv)
  }

  Sys.setenv(LEAD_REHAB_DIR = filepath)
  message('Your folderpath has been stored in your .Renviron. \nAccess it with Sys.getenv("LEAD_REHAB_DIR"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
}

#' Get the filepath for a rehab file/folder
#'
#' @param file The file or folder to get the filepath for. Options:
#'
#'  * `NULL` (the default): get the main folder filepath
#'  * `rehab cases`: get the rehab cases spreadsheet
#'  * `bids`: get the bid days folder
#'  * `neighborly`: get neighborly data in archive folder
#'  * `contractors`: get contractor data
#'
#' @return A character string
#' @export
#'
#' @examples
#' # Get the bid folder
#' get_rehab_filepath("bids")
get_rehab_filepath <- function(file = NULL) {
  fp <- Sys.getenv("LEAD_REHAB_DIR")

  if (nchar(fp) == 0) {
    message('No folderpath detected. Install the path to your rehab folder with install_rehab_folder(). \nRestart R or run `readRenviron("~/.Renviron")`')
  }

  if (file == "rehab cases") {
    fp <- file.path(fp, "Rehab Cases by Program Year.xlsx")
  } else if (file == "bids") {
    fp <- file.path(fp, "Bid Days/")
  } else if (file == "neighborly") {
    fp <- file.path(fp, "archive")
    fp <- list.files(fp,
                     pattern = "DataExport_.*.xlsx",
                     full.names = TRUE)
  } else if (file == "contractors") {
    fp <- file.path(fp, "contractors")
    fp <- list.files(fp,
                     pattern = "contractor-docs.xlsx",
                     full.names = TRUE)
  } else {
    fp
  }

  fp
}

