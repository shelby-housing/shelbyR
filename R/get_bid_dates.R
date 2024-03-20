#' Get bid dates & case ids
#'
#' @param program lead or rehab
#'
#' @export
get_bid_dates <- function(program) {
  logs <- get_rehab_data("neighborly logs")
  bids <- logs |>
    filter(str_detect(note, "Bid Open Date")) |>
    mutate(
      program = {{ program }},
      bid_open_date = str_extract(note, "(?<=Bid Open Date (set|from  \\d{1,2}/\\d{1,2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} [AP]M) to )\\d{1,2}/\\d{1,2}/\\d{4}"),
      bid_close_date = str_extract(note, "(?<=Bid Close Date (set|from  \\d{1,2}/\\d{1,2}/\\d{4} \\d{1,2}:\\d{2}:\\d{2} [AP]M) to )\\d{1,2}/\\d{1,2}/\\d{4}"),
      changed_date = str_extract(note, "(?<=Changed Bid Open Date from  )\\d{1,2}/\\d{1,2}/\\d{4}"),
      across(ends_with("_date"), ~ mdy(.))
    ) |>
    select(case_id, program, bid_open_date, bid_close_date, changed_date) |>
    arrange(case_id, bid_close_date)
  bids <- bids |>
    mutate(bid_length = bid_close_date - bid_open_date) |>
    filter(!(bid_open_date %in% changed_date) & days(bid_length) > days(2), .by = case_id) |>
    select(-changed_date) |>
    distinct()
  bids <- bids |>
    mutate(
      bid_count = row_number(),
      final_bid = case_when(max(bid_count) == bid_count ~ "final bid", .default = "rebid"),
      .by = case_id
    )
}
