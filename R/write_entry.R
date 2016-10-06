#' Write an entry to a csv file
#'
#' Verify an entry and then write it to a csv file
#'
#' @param entry An entry data.frame
#' @param path A character string specifying a file path
#' @return Invisibly returns TRUE if successful
write_entry <- function(entry, path) {
  success <- verify_entry(entry)
  write.csv(entry, file = path)
  invisible(success)
}
