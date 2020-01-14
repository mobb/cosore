# User-side functions

#' Extract one table (\code{description}, \code{data}, etc.) of all datasets
#'
#' @param table Name of table to extract, character
#' @param datasets Optional character vector of dataset names
#' @return A \code{\link{data.frame}}.
#' @export
csr_table <- function(table, datasets = list_datasets()) {
  stopifnot(is.character(table))
  stopifnot(length(table) == 1)
  stopifnot(is.character(datasets))

  all <- list()
  for(ds in datasets) {
    dd <- data_dir(ds)
    f <- file.path(dd, paste0(table, "_", ds, ".RDS"))
    if(file.exists(f)) {
      all[[ds]] <- readRDS(f)
      all[[ds]]$CSR_DATASET <- ds
    } else {
      warning(f, " does not exist")
    }
  }

  rbind_list(all)
}

#' Return an overview of available datasets
#'
#' @return A `data.frame` or `tibble` with data about the constitutent datasets of COSORE.
#' @export
#' @note This is a convenience function, as it simply calls
#' \code{\link{csr_table}} to return the joined \code{description} tables.
csr_database <- function() {
  csr_table("description")
}

#' Return an individual dataset
#'
#' @param dataset Name of dataset, character
#' @return A list with (at least) elements:
#' \item{description}{Contents of \code{DESCRIPTION.txt} file}
#' \item{contributors}{Contents of \code{CONTRIBUTORS.txt} file}
#' \item{ports}{Contents of \code{PORTS.txt} file}
#' \item{data}{Continuous soil respiration data, parsed into a \code{data.frame}}
#' \item{diagnostics}{Diagnostics on the data parsing and QC process}
#' \item{ancillary}{Ancillary site information}
#' @export
csr_dataset <- function(dataset) {
  stopifnot(is.character(dataset))
  stopifnot(length(dataset) == 1)

  # find data dir
  # list files, extracting table name
  # for each file, read into list element
  dd <- data_dir(dataset)
  x <- list(dataset_name = dataset)
  for(f in list.files(path = dd, full.names = TRUE)) {
    tab <- strsplit(basename(f), split = "_")[[1]][1]
    x[[tab]] <- readRDS(f)
  }
  x
}


#' Extract contact emails
#'
#' @param all All data, a list of lists
#' @return A pasted string of emails, separated by semicolons.
#' @export
csr_emails <- function(all) {
  paste(sapply(all, function(x) x$contributors$CSR_EMAIL[1]), collapse = ";")
}
