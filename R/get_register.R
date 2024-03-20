#' Get register's data
#'
#' @param vec Vector of search items for one field (such as a list of addresses).
#' @param field Search field to use. Options:
#'
#'  * `address` (the default)
#'  * `record_number`
#'  * `parcel`
#'
#' @param cache Use cache directory.
#'
#' @import rvest purrr dplyr stringr
#' @importFrom readr read_rds write_rds
#'
#' @return dataframe with search results
#' @export
get_register <- function(vec, field = "address", cache = TRUE) {
  # Each item in list will be used as an id.
  vec <- unique(vec)
  names(vec) <- vec

  # Use cache
  if (cache) {

    cache_dir <- shelbyR_cache_dir("registers")

    if (file.exists(cache_dir)) {

      file_loc <- file.path(cache_dir, "records.rds")

      # Searched for variables already in cache
      if (file.exists(file_loc)) {

        cached <- readr::read_rds(file_loc)
        matched <- cached |> filter(field == {{ field }} & id %in% vec)
        matched_vars <- unique(matched$id)

        if (length(matched_vars) == length(vec)) {

          message("All variables found in cache. Retrieving cached data. For a fresh search, set cache = FALSE.")
          return(matched)

        } else {

          # No matches found in cache, search Register
          if (length(matched_vars) == 0) {

            message("Retrieving data from Register's website. Saving results in cache.")
            new_vars <- vec
            dat <- suppressMessages(load_register(new_vars, field))

            if (ncol(dat) != 10) {
              stop("No results from cache or Register's website.")
            } else {

              new_cache <- rbind(cached, dat) |> distinct()
              readr::write_rds(new_cache, file_loc)
              out <- rbind(matched, dat)

            }

            # Some matches found in cache, search register
          } else {
            message("Retrieving data from cache and Register's website. Saving results in cache.")
            new_vars <- vec[!vec %in% matched_vars]
            dat <- suppressMessages(load_register(new_vars, field))

            # No new results, return cache results
            if (ncol(dat) != 10) {
              out <- matched
            } else {

              new_cache <- rbind(cached, dat) |> distinct()
              readr::write_rds(new_cache, file_loc)
              out <- rbind(matched, dat)

            }

          }

          # Check for variables not found in cache or Register
          missing_vars <- vec[!vec %in% out$id]
          if (length(missing_vars)) {
            missed <- str_flatten_comma(missing_vars, last = " or ")
            message(paste0("No results found for: ", missed, ". Returning remaining values."))
          }

          return(out)

        }

      } else {

        out <- load_register(vec, field)
        readr::write_rds(out, file_loc)
        return(out)

      }
    }
  } else {
    load_register(vec, field)
  }
}

load_register <- function(vec, field) {
  # Get search form from register's website
  url <- "https://search.register.shelby.tn.us/search/content.php"
  html <- read_html(url)
  sesh <- session(url)
  form <- html_form(html)[[1]]

  # Create a list of requests to send
  query <- map(vec, function(x) {

    if (field == "address") {
      q <- html_form_set(form, Address = x)
    }

    if (field == "record_number") {
      q <- html_form_set(form, inst_num = x)
    }

    if (field == "parcel") {
      q <- html_form_set(form, Parcel = x)
    }

    q

  })

  # Submit requests, turn results into a table
  dat <- map(query, function(x) {
    dat <- session_submit(sesh, form = x) |> html_table()
    dat <- dat[[14]]
  })

  # Bind results
  dat <- dat |>
    purrr::keep(\(x) ncol(x) != 1) |>
    list_rbind(names_to = "id") |>
    janitor::clean_names()

  if (ncol(dat) != 10) {
    dat
  } else {
    dat <- dat |>
      rename(address = prop_description,
             record_number = record_info,
             record_date = rec_date,
             record_type = instrument_type) |>
      mutate(across(everything(), ~ na_if(.x, "") |>  str_squish()),
             across(c(grantor, grantee, address), ~ str_to_title(.x))) |>
      mutate(field = {{ field }}, .before = "id") |>
      filter(record_number != "Record Info") |>
      select(-image)
  }
}

#' Filter register data for Shelby County
#'
#' @param df Unfiltered register's data
#'
#' @export
filter_register <- function(df) {
  df |>
    filter(if_any(c("grantor", "grantee"), ~ str_detect(., c("Shelby County"))))
}
