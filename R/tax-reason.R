#' @title Tax Reason
#' @description Desc
#' @return a character string

#' @rdname tax_reason
#' @export
prepare_tax_reason <- function(path){

  # GET DATA ----------------------------------------------------------------

  # Source: King County Assessor

  string <- "
FS = senior citizen exemption
EX = exempt
OP = operating
NP = non profit exemption
CU = open space exemption
HI = home improvement exemption
HP = historic property exemption
MX = more than one reason applies
"


  # WRITE -------------------------------------------------------------------

  writeLines(string, path)


  # RETURN ------------------------------------------------------------------

  tax_reason_prep_status <- get_modified_time(path)

  return(tax_reason_prep_status)

}

#' @rdname tax_reason
#' @export
make_tax_reason <- function(path){

  tax_reason <- readr::read_lines(path) %>%
    stringr::str_c(collapse = "\n") %>%
    parse_lu_string(col_sep = "\\s=\\s", row_sep = "\n",join_name = "TAX_REASON","TAX_REASON_DESC")

  return(tax_reason)
}
