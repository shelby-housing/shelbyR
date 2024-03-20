#' Get rehab data
#'
#' @param file The file to call. See shelbyR_raw_paths$file for all
#'
#' @import readxl lubridate
#' @importFrom janitor clean_names
#' @export
get_rehab_data <- function(file) {
  df <- shelbyR_read_dataraw(file)

  if (file == "rehab cases") {
    df <- df |>
      select(street_number:parcel) |>
      mutate(bid_open = bid_date - days(14), .before = bid_date) |>
      arrange(bid_date)

  } else if (file == "cancelled") {
    df <- df |>
      select(1:2) |>
      mutate(across(where(is.character), ~ str_squish(.x)))

  } else if (file == "queue") {
    df <- df |>
      select(neighborly_id, location:parcel) |>
      clean_address(address) |>
      janitor::remove_empty("cols")

  } else if (file == "contractor names") {
    df <- df |>
      select(id, nickname)

  } else if (file == "contractor docs") {
    df <- df |>
      rename(
        expiry_eoc = shelby_county_eoc_num,
        expiry_losb = shelby_county_losb_num,
        expiry_sams = samuei_number,
        expiry_general_liability = date_general_liability,
        expiry_auto = auto_insurance_date,
        expiry_workers_comp = date_workers_comp,
        expiry_tn = state_tn_contractor_license_date,
        expiry_shelby = shelby_county_business_license,
        expiry_epa_rrp = epahudrrp_firm_cert,
        expiry_tdec = tdec_lead_firm_cert,
      ) |>
      select(id, starts_with("expiry"))

    # Join together EOC & LOSB since you only need one or the other.
    raw <- raw |>
      unite(expiry_eoc_losb, c(expiry_eoc, expiry_losb), sep = "; ") |>
      mutate(expiry_eoc_losb = na_if(expiry_eoc_losb, "NA; NA"))

    # Start doc summary
    no_docs <- raw |>
      filter(if_all(starts_with("expiry_"), is.na))
    no_docs$doc_status <- "missing all docs"

    lead_docs <- raw |>
      filter(if_all(starts_with("expiry_"), ~ !is.na(.)))
    lead_docs$doc_status <- "has all rehab & lead docs"

    rehab_docs <- raw |>
      filter(if_all(c(starts_with("expiry_"), -expiry_tdec), ~ !is.na(.))) |>
      anti_join(lead_docs)
    rehab_docs$doc_status <- "has all rehab docs"

    some_docs <- anti_join(raw, rbind(no_docs, lead_docs, rehab_docs))
    some_docs$doc_status <- "missing some docs"

    # join together
    some_all_docs <- rbind(some_docs, lead_docs, rehab_docs)
    all_docs <- rbind(no_docs, some_all_docs)

    # eoc losb annoyance
    doc_summary <- all_docs |>
      mutate(eoc_losb_status = case_when(
        is.na(expiry_eoc_losb) ~ "no eoc or losb",
        str_starts(expiry_eoc_losb, "NA") ~ "has losb",
        str_ends(expiry_eoc_losb, "NA") ~ "has eoc",
        .default = "has eoc and losb"
      )) |>
      select(id, doc_status, eoc_losb_status) |>
      distinct()

    detailed_docs <- some_all_docs |>
      left_join(doc_summary |> select(id, eoc_losb_status), by = "id") |>
      separate_wider_delim(
        expiry_eoc_losb, delim = "; ", names = c("expiry_eoc", "expiry_losb")
      ) |>
      mutate(across(c(expiry_eoc, expiry_losb), ~ na_if(., "NA") |> ymd()))

    detailed_docs <- detailed_docs |>
      pivot_longer(starts_with("expiry_"),
                   names_to = "doc",
                   names_prefix = "expiry_",
                   values_to = "expiry") |>
      mutate(
        expires_in = expiry - now()
      )

    missing_expired_docs <- detailed_docs |>
      mutate(
        missing = case_when(
          is.na(expiry) & !(doc %in% c("eoc", "losb", "tdec")) ~ doc
        ),
        expired = case_when(expiry < now() ~ paste0(doc, " (", abs(round(expires_in, 0)), " days ago)")),
        expiring_soon = case_when(
          !is.na(expired) ~ NA_character_,
          expires_in <= days(30) ~ paste0(doc, " expires in ", round(expires_in, 0), " days")
        )
      )

    overview <- missing_expired_docs |>
      summarise(
        doc_status = unique(doc_status),
        eoc_losb_status = unique(eoc_losb_status),
        missing = str_flatten_comma(missing, na.rm = TRUE),
        expired = str_flatten_comma(expired, na.rm = TRUE),
        expiring_soon = str_flatten_comma(expiring_soon, na.rm = TRUE),
        .by = id
      )
    overview <- overview |> mutate(across(c(expired, missing, expiring_soon), ~ na_if(., "")))

  } else if (file == "contractor info") {
    df <- df |>
      select(id:created_by,
             types,
             gc_number,
             mbe:section3contractor,
             num_users,
             status)

  } else if (file %in% c("neighborly rehab", "neighborly lead")) {
    df <- df |> rename(address = 4)
    df <- df |>
      clean_address(address) |>
      janitor::remove_empty("cols")

  } else if (file == "neighborly logs") {
    df <- df
  }

}
