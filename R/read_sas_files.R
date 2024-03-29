#' Read in a single SAS file
#'
#' Convert names to lowercase and optionally remove labels.
#'
#' @param path The path to a \code{.sas7bdat} file.
#' @param var_labs Retain the SAS variable labels?
#' @export
read_sas_file <- function(path, var_labs = FALSE) {
  start_msg <- paste0("Reading ", basename(path), "...")
  message(start_msg, appendLF = FALSE)
  end_msg <- paste(" Done.")
  dat <- haven::read_sas(path)
  names(dat) <- tolower(names(dat))
  if (var_labs) {
    message(end_msg)
    return(dat)
  }
  for (name in names(dat)) {
    attr(dat[[name]], "label") <- NULL
  }
  message(end_msg)
  return(dat)
}


#' Read in all of the SAS files in a directory into a list
#'
#' @param directory The directory containing \code{.sas7bdat} files.
#' @param var_labs Retain the SAS variable labels?
#' @export
read_sas_files <- function(directory, var_labs = FALSE) {
  filenames <- list.files(directory, pattern = '\\.sas7bdat$')
  paths <- file.path(directory, filenames)
  out <- lapply(paths, read_sas_file, var_labs)
  names(out) <- tools::file_path_sans_ext(filenames)
  return(out)
}