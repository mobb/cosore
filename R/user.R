# User-side functions

#' Extract one table (\code{description}, \code{data}, etc.) of all datasets
#'
#' @param all All data, a list of lists
#' @param table Name of table to extract, character
#' @return A \code{\link{data.frame}}.
#' @export
csr_table <- function(all, table) {

  extract <- function(x, table) {
    if(is.null(x[[table]])) { return(NULL) }
    if(nrow(x[[table]]) == 0) { return(NULL) }

    x[[table]]$CSR_DATASET <- x$description$CSR_DATASET
    x[[table]]
  }

  rbind_list(lapply(all, extract, table = table))
}

#' Return an overview of available datasets
#'
#' @param all All data, a list of lists
#'
#' @return A `data.frame` or `tibble` with data about the constitutent datasets of COSORE.
#' @export
#' @note This is a convenience function, as it simply calls
#' \code{\link{csr_table}} to return the joined \code{description} tables.
csr_database <- function(all) {
  csr_table(all, "description")
}

#' Return an individual dataset
#'
#' @param all All data, a list of lists
#' @param dataset Name of dataset, character
#' @return A list with (at least) elements:
#' \item{description}{Contents of \code{DESCRIPTION.txt} file}
#' \item{contributors}{Contents of \code{CONTRIBUTORS.txt} file}
#' \item{ports}{Contents of \code{PORTS.txt} file}
#' \item{data}{Continuous soil respiration data, parsed into a \code{data.frame}}
#' \item{diagnostics}{Diagnostics on the data parsing and QC process}
#' \item{ancillary}{Ancillary site information}
#' @export
csr_dataset <- function(all, dataset) {
  if(!dataset %in% names(all)) {
    stop("Unknown dataset")
  }
  all[[dataset]]
}


#' Extract contact emails
#'
#' @param all All data, a list of lists
#' @return A pasted string of emails, separated by semicolons.
#' @export
csr_emails <- function(all) {
  paste(sapply(all, function(x) x$contributors$CSR_EMAIL[1]), collapse = ";")
}
