#' @title Public Owner Names Category Key
#' @description Desc
#' @return a character string

#' @rdname public_owner_name_category_key
#' @export
prepare_public_owner_name_category_key <- function(path){

  # GET DATA ----------------------------------------------------------------

  # Source: Google Drive

  categories_gs <- googlesheets::gs_key("1Uhj9GcPP93hfGehK1dPmxgLbsAXxUOnHXY8PFSfMnRI")

  public_owner_name_category_key <- googlesheets::gs_read(categories_gs,ws = "JOIN_OFFICIAL_NAMES_TARGET")


  # WRITE -------------------------------------------------------------------

  readr::write_rds(public_owner_name_category_key, path)


  # RETURN ------------------------------------------------------------------

  public_owner_name_category_key_prep_status <- get_modified_time(path)

  return(public_owner_name_category_key_prep_status)

}

#' @rdname public_owner_name_category_key
#' @export
make_public_owner_name_category_key <- function(path){

  public_owner_name_category_key <- readr::read_rds(path)

  return(public_owner_name_category_key)
}
