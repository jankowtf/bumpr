#' @title
#' Class: GitVersion.S3 
#'
#' @description
#' Class representing git versions (S3) and its constructor function.
#' 
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{GitVersion.S3}. Mainly intended for rapid prototyping 
#'    purposes
#'    
#' @field version \code{\link{character}}. Version.
#' @field remote_name \code{\link{character}}. Name of remote repository.
#' @field remote_url \code{\link{character}}. URL of remote repository.
#' @field user_email \code{\link{character}}. User e-mail.
#' @field user_name \code{\link{character}}. User name.
#' @field home \code{\link{character}}. User home directory.
#' @return Instance of class \code{GitVersion.S3}.
#' @example inst/examples/GitVersion.S3.r
#' @seealso \code{
#'   	\link[filesystr]{PackageVersion.S3}
#' }
#' @template author
#' @template references
#' @export
GitVersion.S3 <- function(
  .x,
  version = character(),
  remote_name = "origin",
  remote_url = character(),
  user_email = character(),
  user_name = character(),
  home = Sys.getenv("HOME")
) {
  if (!missing(.x)) {
    class(.x) <- c("GitVersion.S3", class(.x))
    out <- .x
  } else {
    out <- structure(
      list(version = version, remote_name = remote_name, 
           remote_url = remote_url, user_email = user_email, 
           user_name = user_name, home = home
      ),
      class = c("GitVersion.S3", "list")
    )
  }
  return(out)
}
