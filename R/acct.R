#' @title Real Property Account Extract
#' @description Account information for owners of parcels in King County.
#' @note Data source: King County Assessor's Office (restricted access)
#' @param path
#' @rdname acct
#' @export
make_acct <- function(path){

  acct <- readr::read_csv(path) %>%
    janitor::clean_names("screaming_snake")

  return(acct)
}

#' @title Real Property Account Extract Ready
#' @description Account information for owners of parcels in King County.
#' @note Data source: King County Assessor's Office (restricted access)
#' @param acct desc
#' @param tax_status desc
#' @param tax_reason desc
#' @rdname acct
#' @export
make_parcel_acct_ready <- function(acct, tax_status, tax_reason){

  # MAKE ACCT_READY

  acct_frmt <- acct %>%
    dplyr::mutate(PIN = make_pin(MAJOR, MINOR)) %>%
    dplyr::rename(TAX_STATUS = TAX_STAT,
           TAX_REASON = TAX_VAL_REASON) %>%
    dplyr::left_join(tax_status, by = "TAX_STATUS") %>%
    dplyr::left_join(tax_reason, by = "TAX_REASON") %>%
    dplyr::transmute(PIN,
              TAXPAYER_NAME = str_clean_upper(TAXPAYER_NAME),
              BILL_YR,
              TAX_STATUS = TAX_STATUS_DESC,
              TAX_REASON = TAX_REASON_DESC,
              APPR_IMPS_VAL,
              APPR_LAND_VAL,
              TAXABLE_IMPS_VAL,
              TAXABLE_LAND_VAL
    )

  p_acct_ready <- acct_frmt %>%
    subset_duplicated("PIN") %>%
    dplyr::group_by(PIN) %>%
    tidyr::drop_na() %>% # remove any duplicates PIN records with NAs
    dplyr::slice(1) %>%  # take the first record and discard the rest
    dplyr::ungroup() %>%
    dplyr::bind_rows(subset_duplicated(acct_frmt,"PIN",notin = TRUE)) %>%
    dplyr::arrange(PIN)


  parcel_acct_ready <- p_acct_ready

  return(parcel_acct_ready)
}
