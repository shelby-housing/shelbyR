#' Get the filepath for a rehab file/folder
#'
#' @param file The file or folder to get the filepath for. Options:
#'
#'  * "none" (the default): get the main folder filepath
#'  * "rehab cases": get the rehab cases spreadsheet
#'  * "bids": get the bid days folder
#'
#' @return A character string
#' @export
#'
#' @examples
#' # Get the bid folder
#' get_rehab_filepath("bids")
get_rehab_filepath <- function(file = "none") {
  fp <- Sys.getenv("LEAD_REHAB_FOLDER")

  if (nchar(fp) == 0) {
    message('No folderpath detected. Install the path to your rehab folder with install_rehab_folder(). \nRestart R or run `readRenviron("~/.Renviron")`')
  }

  if (file == "rehab cases") {
    paste0(fp, "Rehab Cases by Program Year.xlsx")
  } else if (file == "bids") {
    paste0(fp, "Bid Days/")
  } else {
    fp
  }
}

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
#' Sys.getenv("LEAD_REHAB_FOLDER")
#' }
install_rehab_folder <- function(filepath) {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if (!file.exists(renv)) {
    file.create(renv)
  }

  Sys.setenv(LEAD_REHAB_FOLDER = filepath)
  message('Your folderpath has been stored in your .Renviron. \nAccess it with Sys.getenv("LEAD_REHAB_FOLDER"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
}

