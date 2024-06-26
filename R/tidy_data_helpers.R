#' Clean a string to Title Case & string squish
#'
#' @param x A string to clean
#' @param case "lower", "upper", "title", or "title up" for title case with three letter segments in uppercase (useful for many abbreviations).
#' @param punct Keep punctuation?
#' @param spaces Keep spaces?
#'
#' @return A Title Case String.
#'
#' @import stringr
#' @export
clean_string <- function(x, case = FALSE, punct = TRUE, spaces = TRUE) {

  if (punct == FALSE) {
    x <- str_replace_all(x, "[:punct:]|[:symbol:]", " ")
  }

  if (case == "title") {
    x <- str_to_title(x)
  } else if (case == "title up") {
    x <- str_to_title(x)
    x <- str_replace_all(x, "\\b[\\w[:punct:]]{3}\\b", toupper)
  } else if (case == "lower") {
    x <- str_to_lower(x)
  } else if (case == "upper") {
    x <- str_to_upper(x)
  } else {
    x
  }

  if (spaces == FALSE) {
    x <- str_remove_all(x, "\\s")
  }

  x <- str_squish(x)
}

#' Clean columns and remove empty columns
#'
#' @param df A data frame to clean
#' @param sort Sort the columns by name
#' @param all_character Change all columns to character type?
#'
#' @return A cleaned dataframe.
#' @export
#'
#' @import dplyr
#' @importFrom janitor clean_names remove_empty
clean_columns <- function(df, sort = FALSE, all_character = FALSE) {
  df <- df %>% clean_names() %>% remove_empty("cols")

  if (sort == TRUE) {
    df <- df %>% select(sort(names(.)))
  }

  if (all_character == TRUE) {
    df %>% mutate(across(everything(), ~ as.character(.x)))
  }

  df
}

#' Cleaner Addresses
#'
#' @param df dataframe containing addresses and "street_number" & "street_name" columns
#'
#' @return dataframe with cleaned address columns and extra columns removed.
#' @export
cleaner_adr <- function(df) {
  df |> unite(address, street_number, street_name, sep = " ") |> clean_address(address) |> select(street_number, street_name, everything()) |> janitor::remove_empty("cols")
}

#' Clean / Separate an address column
#'
#' @param df dataframe containing addresses
#' @param adr_col column containing addresses
#'
#' @import purrr
#'
#' @return dataframe with cleaned address columns and original address column
#' @export
clean_address <- function(df, adr_col) {

  # zip code
  t_zip <- " \\d{5}( \\d{4})?"

  # state
  t_state <- c(
    "al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga",
    "hi", "id", "il", "in", "ia", "ks", "ky", "la", "me", "md",
    "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj",
    "nm", "ny", "nc", "nd", "oh", "ok", "or", "pa", "ri", "sc",
    "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi"
    # "wy"
  )

  # place/city
  t_place <- str_c("(?<!\\d( (n|s|e|w|west|new|south))? )(",
                   str_flatten(c(
                     "memphis",
                     "collierville",
                     "(?<!(ral[ei]{2}gh|old|cuba) )millington",
                     "cordova",
                     "eads",
                     "(?<!old )brownsville",
                     "austin",
                     "houston",
                     "friendswood"
                   ), collapse = "|"),
                   ")")

  # building
  t_bldg <- "(?<=bldg )\\w+"
  r_bldg <- "bldg \\w+"

  # po box
  t_box <- "p( )?o box \\d+"

  # unit/suite
  t_unit <- str_c("(?<!(hwy|highway))",
                  "(", str_flatten(c(
                    "s(ui)?te", "apt", "unit"
                  ), collapse = "|"), ")? \\d+")
  r_unit <- str_c(
    "(?<!(hwy|highway))(",
    str_flatten(c("s(ui)?te", "apt", "unit"), collapse = "|"),
    ")( #)?( )?\\d+.*")

  # street number
  t_st_no <- "\\d+"

  # special street names (only renamed)
  special_tbl <- tidyr::tibble(
    input = c("highway", "ext(ende)?d"),
    output = c("hwy", "ext")
  )
  special_tbl$input <- paste0("\\b", special_tbl$input, "\\b")
  r_special <- purrr::set_names(special_tbl$output, special_tbl$input)

  # street ending
  # abbreviate endings
  end_tbl <- tidyr::tibble(
    input = c(
      "avenue",
      "boulevard",
      "circle",
      "circle",
      "court",
      "cove",
      "drive",
      "lane",
      "place",
      "road",
      "street",
      "way"
    ),
    output = c(
      "ave",
      "blvd",
      "cir",
      "cl",
      "ct",
      "cv",
      "dr",
      "ln",
      "pl",
      "rd",
      "st",
      "wy"
    )
  )
  end_tbl$input <- paste0("(?<!^)\\b", end_tbl$input, "\\b$")
  r_end <- purrr::set_names(end_tbl$output, end_tbl$input)
  # remove endings
  t_end <- as.vector(end_tbl$output)

  # street directions
  # abbreviate directions
  dir_tbl <- tidyr::tibble(
    input = c("north", "south", "east", "west"),
    output = c("n", "s", "e", "w")
  )
  dir_tbl$input <- paste0("^", dir_tbl$input, "\\b(?!$)")
  r_dir <- purrr::set_names(dir_tbl$output, dir_tbl$input)
  # remove directions
  t_dir <- dir_tbl[, "output"] |> add_row(output = c("sw", "ne", "se", "nw"))
  t_dir <- as.vector(t_dir$output)

  df |>
    mutate(
      # create temporary address column
      temp_adr = clean_string({{ adr_col }}, case = "lower", punct = FALSE),
      # zip codes
      zip = extract_address_part(temp_adr, "zip", t_zip),
      temp_adr = remove_address_part(temp_adr, "zip", t_zip),
      # state
      state = extract_address_part(temp_adr, "state", t_state),
      temp_adr = remove_address_part(temp_adr, "state", t_state),
      # place/city
      city = extract_address_part(temp_adr, "place", t_place),
      temp_adr = remove_address_part(temp_adr, "place", t_place),
      # building
      building = extract_address_part(temp_adr, "building", t_bldg),
      temp_adr = remove_address_part(temp_adr, "building", r_bldg),
      # po box
      po_box = extract_address_part(temp_adr, "box", t_box),
      po_box = str_extract(po_box, "\\d+"),
      temp_adr = remove_address_part(temp_adr, "box", t_box),
      # unit
      unit = extract_address_part(temp_adr, "unit", t_unit),
      temp_adr = remove_address_part(temp_adr, "unit", r_unit),
      # street number
      street_number = extract_address_part(temp_adr, "st_no", t_st_no),
      temp_adr = remove_address_part(temp_adr, "st_no", t_st_no),
      # special street names
      temp_adr = str_replace_all(temp_adr, r_special),
      # street ending
      temp_adr = str_replace_all(temp_adr, r_end),
      street_end = extract_address_part(temp_adr, "end", t_end),
      temp_adr = remove_address_part(temp_adr, "end", t_end),
      # directions
      temp_adr = str_replace_all(temp_adr, r_dir),
      dir_1 = extract_address_part(temp_adr, "dir_1", t_dir),
      temp_adr = remove_address_part(temp_adr, "dir_1", t_dir),
      dir_2 = extract_address_part(temp_adr, "dir_2", t_dir),
      temp_adr = remove_address_part(temp_adr, "dir_2", t_dir),
      # reformat
      across(c(temp_adr, street_end, starts_with("dir_")), ~ str_to_title(.x)),
      state = str_to_upper(state),
      street_name = temp_adr
    ) |>
    select(-temp_adr)

}

#' Extract address parts
#'
#' @param x temporary address column
#' @param part Part of the address to extract
#' @param extract replace with this vector
#'
#' @return A vector
extract_address_part <- function(x, part = NULL, extract) {
  if (length(extract) > 1) {
    extract <- str_c("\\b(", str_flatten(extract, collapse = "|"), ")\\b")
  }

  if (part %in% c("dir_1", "st_no")) {
    extract <- str_c("^", extract, "(?!$)")
  } else if (part %in% c("zip", "state", "place", "unit", "dir_2", "end")) {
    extract <- str_c("(?<!^)", extract, "$")
  } else if (part %in% "box") {
    extract <- str_c("^", extract, "$")
  } else {
    extract
  }

  str_extract(x, extract) |> str_squish()
}

#' Remove address parts
#'
#' @param x Temporary address column
#' @param part Part of the address to extract
#' @param extract replace with this vector
#'
#' @return A vector
remove_address_part <- function(x, part, extract) {
  if (length(extract) > 1) {
    extract <- str_c("\\b(", str_flatten(extract, collapse = "|"), ")\\b")
  }

  if (part %in% c("dir_1", "st_no")) {
    extract <- str_c("^", extract, "(?!$)")
  } else if (part %in% c("zip", "state", "place", "unit", "dir_2", "end")) {
    extract <- str_c("(?<!^)", extract, "$")
  } else if (part %in% "box") {
    extract <- str_c("^", extract, "$")
  } else {
    extract
  }

  str_remove(x, extract) |> str_squish()

}
