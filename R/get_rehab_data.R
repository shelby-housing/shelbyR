#' Get rehab data
#'
#' @param dataset The dataset to call
#'    * `bids`
#'    * `FY23`
#'    * `cancelled`
#'    * `queue`
#'    * `contractors`
#'    * `neighborly cases`
#'    * `neighborly logs`
#'
#' @import readxl lubridate
#' @importFrom janitor clean_names
#' @export
get_rehab_data <- function(dataset) {

  if (dataset %in% c("bids", "FY23", "cancelled")) {
    fp <- get_rehab_filepath("rehab cases")

    if (dataset %in% c("bids", "FY23")) {
      df <- read_excel(fp, sheet = 2) |>
        janitor::clean_names() |>
        select(street_number:parcel)

      if (dataset == "bids") {
        df <- df |>
          mutate(bid_open = bid_date - days(14), .before = bid_date) |>
          arrange(bid_date)
      }
    } else if (dataset == "cancelled") {
      df <- read_excel(fp, sheet = "Cancelled") |>
        janitor::clean_names() |>
        select(1:2) |>
        mutate(across(where(is.character), ~ str_squish(.x)))
    } else if (dataset == "queue") {
      df <- read_excel(fp, sheet = 5, skip = 1) |>
        janitor::clean_names() |>
        select(neighborly_id, location:parcel)
      df <- df |>
        clean_address(address) |>
        janitor::remove_empty("cols")
    }


  } else if (dataset %in% c("contractors")) {
    fp <- get_rehab_filepath("contractors")

    df <- read_excel(fp, sheet = 1) |>
      janitor::clean_names() |>
      select(id, nickname)
  } else if (dataset %in% c("neighborly cases", "neighborly logs")) {
    fp <- get_rehab_filepath("neighborly")

    if (dataset == "neighborly cases") {
      df <- read_excel(fp, sheet = 1) |>
        janitor::clean_names() |>
        rename(address = 4)
      df <- df |>
        clean_address(address) |>
        janitor::remove_empty("cols")
    } else if (dataset == "neighborly logs") {
      df <- read_excel(fp, sheet = 2) |>
        janitor::clean_names()
    }

  }

  df
}
