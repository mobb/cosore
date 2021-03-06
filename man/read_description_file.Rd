% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parser.R
\name{read_description_file}
\alias{read_description_file}
\title{Read dataset DESCRIPTION file}
\usage{
read_description_file(dataset_name, file_data = NULL)
}
\arguments{
\item{dataset_name}{Dataset name, character}

\item{file_data}{File data, character vector; optional for testing}
}
\value{
A \code{data.frame} with the following columns:
\item{CSR_DATASET}{Dataset name (internal to COSORE), character}
\item{CSR_SITE_NAME}{Site name, character}
\item{CSR_LONGITUDE}{Decimal longitude (degrees), numeric}
\item{CSR_LATITUDE}{Decimal latitude (degrees), numeric}
\item{CSR_ELEVATION}{Elevation (m), numeric}
\item{CSR_TIMEZONE}{Timezone, character}
\item{CSR_IGBP}{IGBP class, character}
\item{CSR_NETWORK}{Site network name, character}
\item{CSR_SITE_ID}{Site ID in network, character}
\item{CSR_INSTRUMENT}{Measurement instrument, character}
\item{CSR_MSMT_LENGTH}{Measurement legnth (s), numeric}
\item{CSR_FILE_FORMAT}{Data file format, character}
\item{CSR_TIMESTAMP_FORMAT}{Data timestamp format (see \code{\link{strptime}}), character}
\item{CSR_TIMESTAMP_TZ}{Data timestamp timezone, character}
\item{CSR_PRIMARY_PUB}{Primary publication, character}
\item{CSR_OTHER_PUBS}{Other publications, character}
\item{CSR_DATA_URL}{Data URL or DOI, character}
\item{CSR_ACKNOWLEDGMENT}{Acknowledgment text, character}
Infrequently, the \code{description} object may include:
\item{CSR_NOTES}{Notes, character}
\item{CSR_EMBARGO}{Embargo information, character}
}
\description{
Read dataset DESCRIPTION file
}
\keyword{internal}
