#' @title Present Use Recode
#' @description Desc
#' @return a character string

#' @rdname present_use_recode
#' @export
prepare_present_use_recode <- function(path){

  # GET DATA ----------------------------------------------------------------

  # Source: King County Assessor

  present_use_recode <-
    tibble::tribble(
      ~ ORIG, ~ NEW,
      "Srvc","Service",
      "Ctr","Center",
      "Bldg","Building",
      "Nghbrhood","Neighborhood",
      "Prop ","Property ",
      "Soc ", "Social ",
      "Greenhse", "Greenhouse",
      "Nrsry", "Nursery",
      "Hort", "Horticulture",
      "Relig", "Religious",
      "Res ", "Residential ",
      "Gen ", "General ",
      "Warehse", "Warehouse",
      "Billbrd", "Billboard",
      "Fac\\)", "Facility\\)",
      "Curr", "Current",
      "Tmbr", "Timber",
      "Dev ", "Development ",
      "Agric", "Agriculture",
      "Desig", "Designated",
      "Comm", "Commercial"
    )




  # WRITE -------------------------------------------------------------------

  readr::write_csv(present_use_recode, path)


  # RETURN ------------------------------------------------------------------

  present_use_recode_prep_status <- get_modified_time(path)

  return(present_use_recode_prep_status)

}

#' @rdname present_use_recode
#' @export
make_present_use_recode <- function(path){

  present_use_recode <- readr::read_csv(path, col_types = "cc")

  return(present_use_recode)
}
