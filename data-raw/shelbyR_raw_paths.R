## code to prepare `shelbyR_raw_paths` dataset goes here

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

usethis::use_data(shelbyR_raw_paths, overwrite = TRUE)
