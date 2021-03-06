% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parser.R
\name{read_raw_dataset}
\alias{read_raw_dataset}
\title{Read a complete dataset from raw files}
\usage{
read_raw_dataset(dataset_name, raw_data, dataset)
}
\arguments{
\item{dataset_name}{Dataset name, character}

\item{raw_data}{Path to the raw data folder (not in package)}

\item{dataset}{The dataset (metadata only when called)}
}
\value{
A list with (at least) elements:
\item{data}{Continuous soil respiration data, parsed into a \code{data.frame}}
\item{diagnostics}{Diagnostics on the data parsing and QC process}
}
\description{
Read a complete dataset from raw files
}
\note{
This is normally called only from \code{\link{read_dataset}}.
}
