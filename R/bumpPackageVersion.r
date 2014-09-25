#' @title
#' Bump Package Version Number
#'
#' @description 
#' Bumps an R package project to the next version number.
#' 
#' @details
#' Asumptions:
#' \itemize{
#'   \item{\strong{R package project}: } {
#'   
#'      You are using this function to systematically manage the versions 
#'      of an R package project that follows the official conventions
#'      (see \url{http://cran.r-project.org/doc/manuals/r-release/R-exts.html}.
#'   }
#' }
#' 
#' @section Disclaimer:
#' \strong{This package is really new. So please test this function with 
#' repositories that are not crucial for your productive work!}
#' 
#' @param .ns \strong{Signature argument}.
#'    Object containing namespace information.
#' @template threedot
#' @example inst/examples/bumpPackageVersion.r
#' @seealso \code{
#'   	\link[reactr]{bumpPackageVersion-Bumpr.Git.S3-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "bumpPackageVersion",
  signature = c(
    ".ns"
  ),
  def = function(
    .ns = NULL,
    ...
  ) {
    standardGeneric("bumpPackageVersion")       
  }
)

#' @title
#' Bump Package Version Number
#'
#' @description 
#' See generic: \code{\link[reactr]{bumpPackageVersion}}
#'      
#' @inheritParams bumpPackageVersion
#' @param .ns \code{\link{Bumpr.Git.S3}}.
#' @return See method
#'    \code{\link[reactr]{bumpPackageVersion-Bumpr.Git.S3-method}}
#' @example inst/examples/bumpPackageVersion.r
#' @seealso \code{
#'    \link[reactr]{bumpPackageVersion}
#' }
#' @template author
#' @template references
#' @export
#' @import classr
setMethod(
  f = "bumpPackageVersion", 
  signature = signature(
    .ns = "missing"
  ), 
  definition = function(
    .ns,
    ...
  ) {
    
  .ns <- classr::createInstance(cl = "Bumpr.RPackage.S3", 
    obj = list(
      version = character()
    )
  )    
  return(bumpPackageVersion(
    .ns = .ns,
    ...
  ))
  
  }
)

#' @title
#' Bump Package Version Number
#'
#' @description 
#' See generic: \code{\link[reactr]{Bumpr.RPackage.S3}}
#'   	 
#' @inheritParams bumpPackageVersion
#' @param .ns \code{\link{Bumpr.RPackage.S3}}.
#' @return \code{\link{character}}. New version that the package project has 
#'    been bumped to.
#' @example inst/examples/bumpPackageVersion.r
#' @seealso \code{
#'    \link[reactr]{bumpPackageVersion}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bumpPackageVersion", 
  signature = signature(
    .ns = "Bumpr.RPackage.S3"
  ), 
  definition = function(
    .ns,
    ...
  ) {

  return(bump(what = .ns, ...))
    
  }
)
