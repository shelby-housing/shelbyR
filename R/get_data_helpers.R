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
