#' Clean Rehab Spreadsheet
#'
#' @param df Imported rehab spreadsheet
#'
#' @export
clean_rehab <- function(df) {
  df |>
    janitor::clean_names() |>
    select(idis, everything(), -fy) |>
    rename(idis_id = idis) |>
    mutate(across(where(is.character), ~ na_if(.x, "NA")),
           across(where(is.character) & ends_with("_date"), ~ janitor::excel_numeric_to_date(as.numeric(.x))),
           idis_id = as.numeric(idis_id))
}

#' Clean IDIS dataset
#'
#' @param df Imported IDIS data
#'
#' @export
clean_idis <- function(df) {
  df |>
    janitor::clean_names() |>
    mutate(across(where(is.character), ~ na_if(.x, "NA")),
           across(where(is.character) & ends_with("_date"), ~ as_date(.x)),
           idis_id = as.numeric(idis_id))
}
