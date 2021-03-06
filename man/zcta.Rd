% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zcta.R
\name{prepare_zcta}
\alias{prepare_zcta}
\alias{make_zcta}
\title{Zipcode Tabulation Areas (ZCTA)}
\usage{
prepare_zcta(path)

make_zcta(path, king_county)
}
\value{
Returns an \code{\link[sf:sf]{sf object}}.
}
\description{
Boundaries of zip code tabulation areas within King County, Washington.
}
\note{
\itemize{
\item{Data source: \code{\link[tigris:zctas]{tigris::zctas()}}}
}
}
