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

#' @rdname make_with_beep
#'@keywords internal
mb <- drakepkg::make_with_beep

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

#' @keywords internal
g <- function(x){dplyr::glimpse(x)}

#' Google Sheets Read-all
#'
#' @param ss
#' @param delay_length
#' @keywords internal
#' @return
#' @export
gs_read_all <- function(ss, delay_length = 5){
  ws_names <- googlesheets::gs_ws_ls(ss)


  gs_read_delayed <- function(ss, ws){
    result <- googlesheets::gs_read(ss, ws)
    Sys.sleep(delay_length)
    return(result)
  }

  worksheets <- map(ws_names, ~ gs_read_delayed(ss, ws = .x)) %>%
    set_names(ws_names)


  return(worksheets)
}

#' @keywords internal
not_sfc <- function(x) !any(class(x) %in% 'sfc')
