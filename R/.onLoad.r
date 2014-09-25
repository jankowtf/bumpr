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
    setOldClass("Bumpr.GitVersion.S3", 
      prototype = list(
        version = "0.1.0",
        git_repos_name = "origin",
        git_repos_url = character(),
        user_email = character(),
        user_name = character()
      )
    )
    setOldClass("Bumpr.RPackageVersion.S3")
}
