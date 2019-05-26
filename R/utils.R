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

#' @keywords internal
make_pin <- function( major, minor){
  res <- stringr::str_c(stringr::str_pad(string = major, width = 6,side = "left",pad = "0"),
                        stringr::str_pad(string = minor, width = 4,side = "left",pad = "0"))
  return(res)
}

#' @keywords internal
subset_duplicated <- function(x,nm,notin = FALSE){
        if(notin){
                subset(x, (! x[[nm]] %in% x[[nm]][duplicated(x[[nm]])]))
        }else{
                subset(x, x[[nm]] %in% x[[nm]][duplicated(x[[nm]])])
        }
}


# FUNCTIONS: LOGICAL RECODING ----

#' @keywords internal
is_logical_yn <- function(x){all(unique(x) %in% c("Y","N",NA_character_))}

#' @keywords internal
recode_logical_yn <- function(x){ dplyr::if_else(x %in% "Y",TRUE,FALSE,missing = NA)}

#' @keywords internal
is_logical_01 <- function(x){all(unique(x) %in% c(1,0, NA))}

#' @keywords internal
recode_logical_01 <- function(x){ dplyr::if_else(x %in% 1,TRUE,FALSE,missing = NA)}

#' @keywords internal
is_logical_yesno <- function(x){

  strings <- list("yes", "no") %>%
    purrr::map(~ list(snakecase::to_screaming_snake_case,snakecase::to_snake_case,snakecase::to_upper_camel_case) %>%
          purrr::invoke_map_chr(.x)
    ) %>% purrr::flatten_chr() %>%
    purrr::prepend(NA_character_)

  all(unique(x) %in% strings)

}

#' @keywords internal
recode_logical_yesno <- function(x){

  yes <- purrr::map("yes",~ list(snakecase::to_screaming_snake_case,snakecase::to_snake_case,snakecase::to_upper_camel_case) %>%
               purrr::invoke_map_chr(.x)
  ) %>% purrr::flatten_chr()

  dplyr::if_else(x %in% yes,TRUE,FALSE,missing = NA)

}

#' @keywords internal
str_clean_upper <- function(x){stringr::str_to_upper(stringr::str_trim(stringr::str_squish(stringr::str_replace_all(x,"^[:punct:]]",""))))}

#' @keywords internal
st_intersects_any <- function(x,y){
  sf::st_intersects(x,y) %>%
    purrr::map_lgl(~ length(.x)>0)
}

