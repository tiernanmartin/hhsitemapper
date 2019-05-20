#' @title Make the KC Assessor Parcel_lookup Table
#' @description A lookup table for codes used in the parcel data
#' @param parcel_metadata_table desc
#' @param lookup desc
#' @param present_use_recode desc
#' @export
make_parcel_lookup <- function(parcel_metadata_table, lookup, present_use_recode){

  p_lu_orig <- parcel_metadata_table %>%
    dplyr::left_join(lookup, by = "LU_TYPE") %>%
    tidyr::drop_na()

  p_lu_token <- p_lu_orig %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidytext::unnest_tokens(ORIG, LU_DESCRIPTION, to_lower = FALSE)

  p_lu_recoded <- p_lu_token %>%
    dplyr::left_join(present_use_recode, by = "ORIG") %>%
    dplyr::mutate(LU_DESCRIPTION = dplyr::if_else(is.na(NEW),ORIG,NEW)) %>%
    dplyr::select(id,FIELD_NAME,LU_ITEM, LU_DESCRIPTION) %>%
    tidyr::nest(-id) %>%
    dplyr::mutate(FIELD_NAME = purrr::map_chr(data, ~.x %>% dplyr::pull("FIELD_NAME") %>% dplyr::first()),
           LU_ITEM = purrr::map_int(data, ~.x %>% dplyr::pull("LU_ITEM") %>% dplyr::first() %>% as.integer()),
           LU_DESCRIPTION = purrr::map_chr(data, ~.x %>% dplyr::pull("LU_DESCRIPTION") %>% stringr::str_c(collapse = " "))) %>%
    dplyr::select(FIELD_NAME,
           LU_ITEM,
           LU_DESCRIPTION)

  parcel_lookup <- p_lu_recoded

  return(parcel_lookup)
}
