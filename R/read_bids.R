#' Read bids from a certain date.
#'
#' @param bid_date The date of the bid opening & date of the folder to get bidsheets.
#' @param nested Are the bid sheets nested in the folder?
#'
#' @return A table of bid information
#' @export
#'
#' @import tidyr
#' @import purrr
read_bids <- function(bid_date, nested = TRUE) {

  a <- get_rehab_filepath("bids")
  d <- paste0(a, bid_date, "/")

  if (nested == TRUE) {
    filepath <- paste0(d, list.files(d, recursive = TRUE, pattern = "\\d+ - .*.xlsx"))
  } else {
    filepath <- paste0(d, list.files(d, pattern = ".xlsx"))
  }

  map(filepath, read_bid) %>% list_rbind()
}

#' Get bids from a file.
#'
#' @param filepath The filepath to get bid information from
#'
#' @return A tibble of bid information and details
#'
#' @import dplyr
#' @import stringr
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
read_bid <- function(filepath) {
  f <- filepath

  bid_info <- read_bid_info(f)

  bid_details <- read_bid_details(f)

  bind_cols(bid_info, bid_details)
}

#' Read bid information from a file.
#'
#' @param filepath The filepath to get bid information from.
#'
#' @return A tibble of bid information.
#'
read_bid_info <- function(filepath) {
  b <- readxl::read_excel(filepath,
                          col_names = c("name", "value"),
                          range = "A1:B4") %>%
    pivot_wider() %>%
    janitor::clean_names() %>%
    mutate(case_id = as.numeric(str_extract(case, "^\\d+")),
           write_up_id = as.numeric(str_extract(work_write_up, "^\\d+"))
    ) %>%
    select(case_id,
           write_up_id,
           bid_open_date = bid_open,
           bid_close_date = bid_close)
}

#' Read bid details from a file
#'
#' @param filepath The filepath to get bid details from.
#'
#' @return A tibble of bid details
#'
#' @importFrom janitor remove_empty
read_bid_details <- function(filepath) {
  readxl::read_excel(filepath, skip = 4) %>%
    clean_names() %>%
    remove_empty("cols") |>
    filter(!is.na(item)) %>%
    pivot_longer(cols = c(everything(), -c(id:cost_estimate)),
                 names_to = "contractor_name",
                 values_to = "bid_amount") %>%
    mutate(across(everything(), ~ clean_string(.x, case = "title")),
           contractor_id = as.numeric(str_extract(contractor_name, "(?<=X)\\d+")),
           bid_amount = as.numeric(bid_amount)) %>%
    select(item_id = id,
           spec_id:cost_estimate,
           contractor_id,
           bid_amount)
}
