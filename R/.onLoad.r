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
    setOldClass("Bumpr.Git.S3", 
      prototype = list(
        git_repos = "origin",
        user_email = character(),
        user_name = character()
      )
    )
}
