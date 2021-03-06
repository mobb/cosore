% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_dataset_names}
\alias{check_dataset_names}
\title{Check dataset for name and class consistency with metadata file}
\usage{
check_dataset_names(dataset_name, dataset, field_metadata)
}
\arguments{
\item{dataset_name}{Dataset name, character}

\item{dataset}{An individual dataset}

\item{field_metadata}{Field metadata file, from \code{inst/extdata/CSR_COLUMN_UNITS.csv}}
}
\value{
A count of how many times each metadata entry appeared in the dataset.
}
\description{
Check dataset for name and class consistency with metadata file
}
