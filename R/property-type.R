#' @title Tax Reason
#' @description Desc
#' @return a character string

#' @rdname prop_type
#' @export
prepare_prop_type <- function(path){

  # GET DATA ----------------------------------------------------------------

  # Source: King County Assessor

  string <- "
C = Commercial
K = Condominium
M = Coal & Mineral Rights
N = Mining
R = Residential
T = Timber
U = Undivided Interest
X = Exempt
"


  # WRITE -------------------------------------------------------------------

  writeLines(string, path)


  # RETURN ------------------------------------------------------------------

  prop_type_prep_status <- get_modified_time(path)

  return(prop_type_prep_status)

}

#' @rdname prop_type
#' @export
make_prop_type <- function(path){

  prop_type <- readr::read_lines(path) %>%
    stringr::str_c(collapse = "\n") %>%
    parse_lu_string(col_sep = "\\s=\\s", row_sep = "\n",join_name = "PROP_TYPE","PROP_TYPE_DESC")

  return(prop_type)
}
