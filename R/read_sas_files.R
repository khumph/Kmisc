#' Read in all of the SAS files in a directory
#'
#' @param directory The directory containing \code{.sas7bdat} files.
#' @export
read_sas_files <- function(directory) {
  filenames <- list.files(directory, pattern = '\\.sas7bdat$')
  paths <- file.path(directory, filenames)
  out <- lapply(paths, function(x) {
    dat <- haven::read_sas(x)
    names(dat) <- tolower(names(dat))
    return(dat)
  })
  names(out) <- tools::file_path_sans_ext(filenames)
  return(out)
}


#' Copy all of the files in a directory into another directory
#'
#' Copies files from one directory into another, only copying files that have been updated.
#' @param dir_in The directory with files to copy
#' @param dir_out The directory to copy into. If this doesn't exist it will be created.
#' @param ... Additional arugments passed to fs::dir_ls()
#' @export
cp_dat <- function(dir_in, dir_out, ...) {
  paths_there <- fs::dir_ls(dir_in, ...)

  fs::dir_create(dir_out, recurse = TRUE)
  paths_here <- fs::dir_ls(dir_out, ...)

  files_there_names <- fs::path_file(paths_there)
  files_here_names <- fs::path_file(paths_here)

  files_there_here <- files_here_names %in% files_there_names
  files_here_there <- files_there_names %in% files_here_names

  files_there_info <- fs::file_info(paths_there[files_here_there])
  files_here_info <- fs::file_info(paths_here[files_there_here])

  newer_there <- files_there_info$modification_time > files_here_info$modification_time

  paths_newer_there <- paths_there[files_here_there][newer_there]
  paths_not_here <- paths_there[!files_here_there]
  paths_to_copy <- c(paths_newer_there, paths_not_here)

  fs::file_copy(paths_to_copy, dir_out, overwrite = TRUE)
}
