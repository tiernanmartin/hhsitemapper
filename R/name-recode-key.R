#' @title Tax Reason
#' @description Desc
#' @return a character string

#' @rdname name_recode_key
#' @export
prepare_name_recode_key <- function(path){

  # GET DATA ----------------------------------------------------------------

  # Source: Google Drive

  recode_key_gs <- googlesheets::gs_key("1aInQqXPK3tqrXKd80PXPugR8G7Nysz46tirCTAdKn6s")

  recode_key_list <- gs_read_all(recode_key_gs, delay_length = 6)

  recode_key_list["ABBREVIATIONS"] <- NULL

  recode_key_ngram_long <- recode_key_list %>%
    purrr::map_dfr(~ tidyr::gather(.x, NGRAM_TYPE, WORD, -NAME_NEW, -NAME_ABBR))

  recode_key_ngram_wide <- recode_key_ngram_long %>%
    dplyr::group_by(NGRAM_TYPE) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::spread(NGRAM_TYPE, WORD) %>%
    tidyr::replace_na(list(NAME_NEW = "")) %>%
    dplyr::arrange(row) %>%
    dplyr::select(-row)

  name_recode_key <- recode_key_ngram_wide


  # WRITE -------------------------------------------------------------------

  readr::write_rds(name_recode_key, path)


  # RETURN ------------------------------------------------------------------

  name_recode_key_prep_status <- get_modified_time(path)

  return(name_recode_key_prep_status)

}

#' @rdname name_recode_key
#' @export
make_name_recode_key <- function(path){

  name_recode_key <- readr::read_rds(path)

  return(name_recode_key)
}
