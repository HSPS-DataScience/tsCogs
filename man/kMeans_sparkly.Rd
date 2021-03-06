% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kMeans_sparkly.R
\name{kMeans_sparkly}
\alias{kMeans_sparkly}
\title{kMeans_sparkly}
\usage{
kMeans_sparkly(data, centers = 10)
}
\arguments{
\item{data}{Tibble/Data Frame with the following columns:
\itemize{
\item Account Number (unique identifier)
\item Date
\item Count
}}

\item{centers}{Number of kmeans centers
\itemize{
\item Default is \code{10}
}}
}
\value{
spread tibble
}
\description{
\strong{Intended to create kmeans from sparklyr}
}
\examples{
test
}
