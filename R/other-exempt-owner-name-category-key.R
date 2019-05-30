#' @title Other Exempt Owner Names Category Key
#' @description Desc
#' @return a character string

#' @rdname other_exempt_owner_name_category_key
#' @export
prepare_other_exempt_owner_name_category_key <- function(path){

  # GET DATA ----------------------------------------------------------------

  # Source: Google Drive

  categories_gs <- googlesheets::gs_key("1xRE5A2suzH_KcrrFpqdcX7fguFShokjFchm7khML6sQ")

  other_exempt_owner_name_category_key <- googlesheets::gs_read(categories_gs,ws = "OWNER_NAME_CATEGORIES")

  readr::write_csv(other_exempt_owner_name_category_key, path)


  # WRITE -------------------------------------------------------------------

  readr::write_csv(other_exempt_owner_name_category_key, path)


  # RETURN ------------------------------------------------------------------

  other_exempt_owner_name_category_key_prep_status <- get_modified_time(path)

  return(other_exempt_owner_name_category_key_prep_status)

}

#' @rdname other_exempt_owner_name_category_key
#' @export
make_other_exempt_owner_name_category_key <- function(path){

  other_exempt_owner_name_category_key <- readr::read_csv(path)

  return(other_exempt_owner_name_category_key)
}
