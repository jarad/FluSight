#' Write an entry to a csv file
#'
#' Verify an entry and then write it to a csv file
#'
#' @param entry An entry data.frame
#' @param path A character string specifying a directory for the file
#' @param team_name A character string specifying the team name
#' @param week A scalar numeric indicating the week number or NULL (default).
#'   If NULL, the week will automatically be determined from the current date.
#' @return Invisibly returns TRUE if successful
#' @seealso \code{\link{write_valid_entry}}, \code{\link{read_entry}}
#' @export
write_entry <- function(entry, path, team_name, week=NULL) {
  if (missing(entry    )) stop("Need to specify `entry`.")
  if (missing(path     )) stop("Need to specify `path`.")
  if (missing(team_name)) stop("Need to specify `team_name`.")

  success <- verify_entry(entry)

  filename <- construct_filename(team_name, week)
  file <- file.path(path, filename)

  utils::write.csv(entry, file = file, row.names=FALSE)

  invisible(success)
}





#' Write a valid entry to a csv file
#'
#' This will write a complete valid entry to a csv file so participants can
#' see the structure of the file. The file will be called
#'
#' @param path A character string specifying a directory
#' @return Invisibly returns TRUE if successful
#' @seealso \code{\link{write_entry}}
#' @export
write_valid_entry <- function(path) {
  write_entry(valid_entry, path=path, team_name="teamDefault")
}




#' Construct the file name
#'
#' @param team_name A character string indicating the team name
#' @param week A scalar numeric indicating the week
#' @return A character string for the filename
construct_filename <- function(team_name, week) {
  # Construct filename
  today = Sys.Date()

  if (is.null(week)) week = get_week(today)
  week = formatC(week, width = 2, format="d", flag="0")

  paste0("EW",week,"-",
         team_name,"-",
         format(today, "%Y-%m-%d"),
         ".csv")
}



#' Get the surveillance week
#'
#' @param date A Date object or one that can be coerced to a Date object
#' @return The numeric week number
#' @importFrom MMWRweek MMWRweek
#' @seealso \code{\link{write_entry}}, \code{\link{as.Date}}
get_week <- function(date) {
  warning("Getting a week automatically is an experimental feature. ",
          "Please check the week in the filename.")

  if (!is(date, "Date")) date = as.Date(date)

  MMWRweek::MMWRweek(date-9)$MMWRweek
}
