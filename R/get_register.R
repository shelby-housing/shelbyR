#' Get register's data
#'
#' @param vec Vector of data (for one field) to search
#' @param field The field to search. Options:
#'
#'  * "address" (the default)
#'  * record_number
#'  * parcel
#'
#' @import rvest
#' @import purrr
#' @import dplyr
#' @import stringr
#'
#' @return dataframe with search results
#' @export
get_register <- function(vec, field = "address") {
  names(vec) <- vec

  url <- "https://search.register.shelby.tn.us/search/content.php"
  base <- read_html(url)
  s <- session(url)
  search <- html_form(base)[[1]]

  query <- map(vec, function(x) {

    if (field == "address") {
      q <- html_form_set(search, Address = x)
    }

    if (field == "record_number") {
      q <- html_form_set(search, inst_num = x)
    }

    if (field == "parcel") {
      q <- html_form_set(search, Parcel = x)
    }

    q

  })

  dat <- map(query, function(x) {
    dat <- session_submit(s, form = x) |> html_table()
    dat <- dat[[14]]
  })

  dat <- dat |>
    purrr::keep(\(x) ncol(x) != 1) |>
    list_rbind(names_to = "id") |>
    janitor::clean_names() |>
    rename(address = prop_description,
           record_number = record_info,
           record_date = rec_date,
           record_type = instrument_type) |>
    mutate(across(everything(), ~ na_if(.x, "") |>  str_squish()),
           across(c(grantor, grantee, address), ~ str_to_title(.x))) |>
    filter(record_number != "Record Info") |>
    select(-image)
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
