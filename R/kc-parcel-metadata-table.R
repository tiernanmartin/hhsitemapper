#' @title Make the KC Parcel Metadata Table
#' @description A metadata table used in the parcel data
#' @note Data source: King County Assessor Data Portal (\url{http://info.kingcounty.gov/assessor/datadownload/desc/Parcel.doc})
#' @param path
#' @rdname parcel_metadata_table
#' @export
make_parcel_metadata_table <- function(path){

  parcel_metadata_table <- readr::read_csv(path,col_types = "cccc") %>%
    dplyr::transmute(FIELD_NAME = snakecase::to_screaming_snake_case(FIELD_NAME),
              LU_TYPE = LOOKUP)

  return(parcel_metadata_table)
}
