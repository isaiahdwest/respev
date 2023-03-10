% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complete_input.R
\name{get_purchase_year}
\alias{get_purchase_year}
\title{Fill in Purchase Year}
\usage{
get_purchase_year(.data)
}
\arguments{
\item{.data}{Dataframe of vehicle data}
}
\description{
Fill in a value for vehicles missing a purchase year, assumes
that it's the same as model year
}
\examples{
\dontrun{
get_purchase_year(vehicle_data)
}
}
