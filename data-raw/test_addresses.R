## code to prepare `test_addresses` dataset goes here

# library(readr)
# library(arrow)
#
# raw <- read_csv("data-raw/test_addresses.csv") |> clean_columns(all_character = TRUE)
# raw <- read_parquet(paste0(Sys.getenv("MY_DATA_DIR"), "idis/rehab-231129.parquet"))
#
# middle <- raw |>
#   unite(address, address1:zip, sep = " ", remove = FALSE, na.rm = TRUE)
# middle <- raw |>
#   unite(address, c(street_number, street_name), sep = " ", remove = FALSE, na.rm = TRUE)
#
# middle2 <- middle |>
#   clean_address(address)
#
# usethis::use_data(test_addresses, overwrite = TRUE)
