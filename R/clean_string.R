#' Clean a string to Title Case & string squish
#'
#' @param x A string to clean
#' @param punct Keep punctuation?
#' @param spaces Keep spaces?
#' @param case "lower", "upper", "title", or "title up" for title case with three letter segments in uppercase (useful for many abbreviations).
#'
#' @return A Title Case String.
#'
#' @import stringr
clean_string <- function(x, case = FALSE, punct = TRUE, spaces = TRUE) {
  if (punct == FALSE) {
    x <- str_replace_all(x, "[:punct:]|[:symbol:]", " ")
  } else {
    x
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
#' @importFrom janitor clean_names
#' @importFrom janitor remove_empty
clean_columns <- function(df, sort = FALSE, all_character = FALSE) {
  df <- df %>% clean_names() %>% remove_empty("cols")

  if (sort == TRUE) {
    df <- df %>% select(sort(names(.)))
  } else {
    df
  }

  if (all_character == TRUE) {
    df %>% mutate(across(everything(), ~ as.character(.x)))
  } else {
    df
  }
}
