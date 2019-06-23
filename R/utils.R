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

#' @keywords internal
st_intersect_area <- function(x, y){

  x_sfc <- x %>%
    sf::st_geometry() %>%
    sf::st_transform(sf::st_crs(y))

  area_x <- x_sfc %>% sf::st_area() %>% as.double()

  area_xy <- sf::st_intersection(x_sfc, y) %>% sf::st_area() %>% as.double()

  if(purrr::is_empty(area_xy)){return(as.double(0))}

  overlap_pct <- area_xy %>%
    magrittr::divide_by(area_x) %>%
    as.double() %>%
    round(2)

  return(overlap_pct)
}

#' @keywords internal
st_over <- function(x,y,col){
  idx <- sapply(sf::st_intersects(x,y), function(z) if (length(z)==0) NA_integer_ else z[1])

  y[idx,col][[1]]
}

#' @keywords internal
st_area_ratio <- function(x){

  x$poly_area <- sf::st_area(x)
  x$min_bound_cicle <- lwgeom::st_minimum_bounding_circle(x, nQuadSegs = 10)
  x$min_bound_cicle_area <- sf::st_area(x$min_bound_cicle)
  x$area_ratio <- as.double(x$poly_area/x$min_bound_cicle_area)

  return(x$area_ratio)

}

#' @keywords internal
str_unique_lower <- function(x){

  string <- stringr::str_c(unique(stringr::str_to_lower(stringr::str_replace(x,"_"," "))),collapse = ", ")

  if(length(string) == 0){return("none")}else(return(string))

}

#' @keywords internal
str_count_factor <- function(x){

  if(length(x)==1 & all(is.na(x))){return(NA_character_)}else{

    fct <- factor(x)

    lvls <- levels(fct)

    ln <- length(lvls)

    counts <- tabulate(fct, ln)

    stringr::str_c(purrr::map2_chr(lvls, counts, ~stringr::str_c(.x,": ", .y)), collapse = "; ")

  }
}

#' @keywords internal
first_not_na <- function(x){
        if(all(sapply(x,is.na))){
                as(NA,class(x))
                }else{
                x[!sapply(x,is.na)][1]
        }


}

#' @keywords internal
lesser_of <- function(x,y){
  x <- as.double(x)
  y <- as.double(y)

  dplyr::if_else(x<= y, x,y, missing = x)

}

#' @keywords internal
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA_character_)
}
