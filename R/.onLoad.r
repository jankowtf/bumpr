#' On Load Hook
#'
#' @description 
#' On load hook.
#' 
#' @param libname 
#' @param pkgname
#' @template author
#' @template references
#' @export .onLoad
.onLoad <- function(libname, pkgname) {
    setOldClass("GitVersion.S3")
    setOldClass("RPackageVersion.S3")
}
