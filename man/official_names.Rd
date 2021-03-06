% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/official_names.R
\name{make_official_names}
\alias{make_official_names}
\alias{prepare_official_names_seattle}
\alias{make_official_names_seattle}
\alias{prepare_official_names_kc}
\alias{make_official_names_kc}
\alias{prepare_official_names_wa}
\alias{make_official_names_wa}
\alias{prepare_official_names_us}
\alias{make_official_names_us}
\alias{prepare_official_names_places}
\alias{make_official_names_places}
\alias{prepare_official_names_tribes}
\alias{make_official_names_tribes}
\alias{prepare_official_names_housing_authorities}
\alias{make_official_names_housing_authorities}
\alias{make_official_names_regional_transit_authorities}
\alias{prepare_official_names_special_purpose_districts}
\alias{make_official_names_special_purpose_districts}
\alias{make_official_names_school_districts}
\alias{prepare_official_names_higher_ed_providers}
\alias{make_official_names_higher_ed_providers}
\alias{prepare_official_names_hospitals}
\alias{make_official_names_hospitals}
\title{Official Names}
\usage{
make_official_names(official_names_seattle, official_names_kc,
  official_names_wa, official_names_us, official_names_places,
  official_names_tribes, official_names_housing_authorities,
  official_names_regional_transit_authorities,
  official_names_special_purpose_districts, official_names_school_districts,
  official_names_higher_ed_providers, official_names_hospitals)

prepare_official_names_seattle(path)

make_official_names_seattle(path)

prepare_official_names_kc(path)

make_official_names_kc(path)

prepare_official_names_wa(path)

make_official_names_wa(path)

prepare_official_names_us(path)

make_official_names_us(path)

prepare_official_names_places(path)

make_official_names_places(path)

prepare_official_names_tribes(path)

make_official_names_tribes(path)

prepare_official_names_housing_authorities(path)

make_official_names_housing_authorities(path)

make_official_names_regional_transit_authorities()

prepare_official_names_special_purpose_districts(path)

make_official_names_special_purpose_districts(path)

make_official_names_school_districts(official_names_special_purpose_districts)

prepare_official_names_higher_ed_providers(path)

make_official_names_higher_ed_providers(path)

prepare_official_names_hospitals(path)

make_official_names_hospitals(path)
}
\value{
Returns a \code{\link[tibble:tbl_df]{tbl_df}} object.
}
\description{
The official names of the public offices, departments, and taxing authorities of various levels
of govenment found in Washington State.
}
\note{
The following datasets are stored in the a Github repository which is maintained by the package's author:
\itemize{
\item{Seattle data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/seattle-bureaucracy/data/seattle-bureaucracy.csv}{seattle-bureaucracy.csv}
}
\item{King County data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/king-county-bureaucracy/data/kc-bureaucracy.csv}{kc-bureaucracy.csv}
}
\item{Washington State data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-bureaucracy/data/wa-bureaucracy.csv}{wa-bureaucracy.csv}
}
\item{Federal Government data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/us-bureaucracy/data/us-bureaucracy.csv}{us-bureaucracy.csv}
}
\item{Places data source: \code{\link[tigris:places]{tigris::places}()}
}
\item{Tribes data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-tribes/data/wa-tribes.csv}{wa-tribes.csv}
}
\item{Housing Authorities data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-housing-authorities/data/wa-housing-authorities.csv}{wa-housing-authorities.csv}
}
\item{Regional Transit Authorities: no external data (Sound Transit is the only RTA in King County)
}
\item{Special Purpose Districts & School Districts data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-special-purpose-districts/data/wa-special-purpose-districts.csv}{wa-special-purpose-districts.csv}
}
\item{Higher-Education Providers data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-higher-ed-providers/data/wa-higher-ed-providers.csv}{wa-higher-ed-providers.csv}
}
\item{Hospitals data download: \href{https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-hospitals/data/wa-hospitals.csv}{wa-hospitals.csv}
}
}
}
