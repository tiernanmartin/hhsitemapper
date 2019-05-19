#' @title A set of generic helpful functions
#' @description General purpose functions to make working in R easier.
#' @name utils
#' @import purrr
NULL


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Make With Beep
#'
#' See \code{drakepkg::\link[drakepkg:make_with_beep]{make_with_beep}} for details.
#'
#' @name make_with_beep
#' @export
#' @importFrom drakepkg make_with_beep
NULL


#' Parse a Lookup Table Character String
#'
#' @param string
#' @param col_sep
#' @param row_sep
#' @param join_name
#' @param long_name
#' @keywords internal
#' @return
#' @export
parse_lu_string <- function(string, col_sep, row_sep, join_name, long_name){
  stringr::str_split(string, pattern = row_sep) %>%
    purrr::flatten() %>%
    purrr::keep(~ stringr::str_detect(.x,"")) %>%
    stringr::str_replace_all("\\\n","") %>%
    purrr::map_chr(c) %>%
    purrr::map_df(~.x %>% stringr::str_split(pattern = col_sep) %>% as.data.frame %>% t %>% tibble::as_data_frame() ) %>%
    purrr::set_names(c(join_name,long_name))
}
