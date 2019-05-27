#' @title Other SUitability Characteristics
#' @description Desc
#' @return Returns a \code{tibble}.

#' @rdname other_suitability_characteristics
#' @export
prepare_other_suitability_characteristics <- function(path){

  # GET DATA ----------------------------------------------------------------

  # Source: Google Drive

  other_gs <- googlesheets::gs_key("1a-xqAjyCI3XITm9BxfTdw6UNyoG5r2UyacNzE4N60QU")

  other_suitability_characteristics <- googlesheets::gs_read(other_gs, sheet = "SUIT_OTHER", col_types = "clcc")


  # WRITE -------------------------------------------------------------------

  readr::write_rds(other_suitability_characteristics, path)


  # RETURN ------------------------------------------------------------------

  other_suitability_characteristics_prep_status <- get_modified_time(path)

  return(other_suitability_characteristics_prep_status)

}

#' @rdname other_suitability_characteristics
#' @export
make_other_suitability_characteristics <- function(path){

  other_suitability_characteristics <- readr::read_rds(path)

  return(other_suitability_characteristics)
}
