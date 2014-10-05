#' @title
#' Class: RPackageVersion.S3 
#'
#' @description
#' Class representing R package versions (S3) and its constructor function.
#' 
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{RPackageVersion.S3}. Mainly intended for rapid prototyping 
#'    purposes
#'    
#' @field version \code{\link{character}}. Version.
#' @field lib \code{\link{character}}. Library location.
#' @field path \code{\link{character}}. Path to package project.
#' @return Instance of class \code{RPackageVersion.S3}.
#' @example inst/examples/RPackageVersion.S3.r
#' @seealso \code{
#'   	\link[filesystr]{GitVersion.S3}
#' }
#' @template author
#' @template references
#' @export
RPackageVersion.S3 <- function(
  .x,
  version = character(),
  lib = character(),
  path = character()
) {
  if (!missing(.x)) {
    class(.x) <- c("RPackageVersion.S3", class(.x))
    out <- .x
  } else {
    out <- structure(
      list(version = version, lib = lib, path = path),
      class = c("RPackageVersion.S3", "list")
    )
  }
  return(out)
}
