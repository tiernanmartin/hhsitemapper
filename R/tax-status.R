#' @title Tax Status
#' @description Desc
#' @return a character string

#' @rdname tax_status
#' @export
prepare_tax_status <- function(path){

  # GET DATA ----------------------------------------------------------------

  # Source: King County Assessor

  string <- "T = Taxable; X = Exempt; O = Operating"


  # WRITE -------------------------------------------------------------------

  writeLines(string, path)


  # RETURN ------------------------------------------------------------------

  tax_status_prep_status <- get_modified_time(path)

  return(tax_status_prep_status)

}

#' @rdname tax_status
#' @export
make_tax_status <- function(path){

  tax_status <- readr::read_lines(path) %>%
    stringr::str_c(collapse = "\n") %>%
    parse_lu_string(col_sep = "\\s=\\s",row_sep = ";\\s",join_name = "TAX_STATUS","TAX_STATUS_DESC")

  return(tax_status)
}
