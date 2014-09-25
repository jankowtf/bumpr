#' @title
#' Bump Git Version Number
#'
#' @description 
#' Bumps an R package project to the next Git version number.
#' 
#' @details
#' Asumptions:
#' \itemize{
#'   \item{\strong{R package project}: } {
#'   
#'      You are using this function to systematically manage the versions 
#'      of an R package project that follows the official conventions
#'      (see \url{http://cran.r-project.org/doc/manuals/r-release/R-exts.html}
#'   }
#'   \item{\strong{Local Git repository}: }{
#'   
#'      Your package project is under Git version control, i.e. a local Git 
#'      repository has been created in your package project's root directory.
#'      (look for directory \code{.git} in your package project's root 
#'      directory)
#'   }
#'   \item{\strong{Remote Git repository}: }{
#'   
#'      At the very least one remote repository with name \code{origin}
#'      has been defined for your local Git repository. 
#'      
#'      Additional remote repositories with different 
#'      names are not a problem. You can choose them interactively. 
#'      (Run \code{git remote} in your git shell to find out about your 
#'      remote repositories)
#'   }
#'   \item{\strong{HTTP credentials}: }{
#'   
#'      If you want to push to a GitHub repository or any other remote 
#'      repository that relies on HTTPS for authentication, the function 
#'      assumes that you \strong{are willing to store (at least temporarily) your 
#'      HTTP credentials in this file}:
#'      \code{file.path(Sys.getenv("HOME"), "_netrc")}. 
#'      
#'      Currently only tested for
#'      GitHub repositories as this is the location where the API seems to 
#'      expect HTTP credentials when pushing to such a repository. 
#'      
#'      \strong{However, You can choose to destroy this file after each bump by setting
#'      \code{temp_credentials = TRUE}}.
#'      I will try to find better ways of handling HTTPS credentials in future
#'      releases.
#'   }
#' }
#' 
#' @section Disclaimer:
#' \strong{This package is really new. So please test this function with 
#' repositories that are not crucial for your productive work!}
#' 
#' @section Recommendations:
#' \itemize{
#'   \item{\strong{Initial commit}: }{
#'   
#'      Make sure that you already issued an initial commit for your \strong{local}
#'      repository and pushed this to your remote repository.
#'      The function does have built-in checks for very early stages of a
#'      Git repository (i.e. no commits yet, no \code{.gitignore} file yet, 
#'      no branches on the remote repository yet), but I would not consider 
#'      this completely stable and tested yet. If you want to check out what 
#'      the function does in such early stages, I would recommend testing it
#'      with a toy Git/GitHub repository first
#'   }
#' }
#' 
#' @param temp_credentials \code{\link{logical}}.
#'    \code{TRUE}: delete HTTPS credentials after each bump;
#'    \code{FALSE}: permanently store HTTPS credentials in \code{_netrc} file.
#'    See details.   
#' @param .ns \strong{Signature argument}.
#'    Object containing namespace information.
#' @template threedot
#' @example inst/examples/bumpGitVersion.r
#' @seealso \code{
#'   	\link[reactr]{bumpGitVersion-Bumpr.Git.S3-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "bumpGitVersion",
  signature = c(
    ".ns"
  ),
  def = function(
    temp_credentials = FALSE,
    .ns = NULL,
    ...
  ) {
    standardGeneric("bumpGitVersion")       
  }
)

#' @title
#' Bump Git Version Number
#'
#' @description 
#' See generic: \code{\link[reactr]{bumpGitVersion}}
#'      
#' @inheritParams bumpGitVersion
#' @param .ns \code{\link{Bumpr.Git.S3}}.
#' @return See method
#'    \code{\link[reactr]{bumpGitVersion-Bumpr.Git.S3-method}}
#' @example inst/examples/bumpGitVersion.r
#' @seealso \code{
#'    \link[reactr]{bumpGitVersion}
#' }
#' @template author
#' @template references
#' @export
#' @import classr
setMethod(
  f = "bumpGitVersion", 
  signature = signature(
    .ns = "missing"
  ), 
  definition = function(
    temp_credentials,
    .ns,
    ...
  ) {
    
  .ns <- classr::createInstance(cl = "Bumpr.Git.S3", 
    obj = list(
      version = character(),
      git_repos = "origin",
      user_email = character(),
      user_name = character(),
      home = Sys.getenv("HOME")
    )
  )    
  return(bumpGitVersion(
    temp_credentials = temp_credentials,
    .ns = .ns,
    ...
  ))
  
#   ## From and to //
#   from <- unname(read.dcf("DESCRIPTION", field = "Version")[1,1])
#   if (is.na(from)) {
#     stop("Invalid version in DESCRIPTION")
#   }
    
  }
)

#' @title
#' Bump Git Version Number
#'
#' @description 
#' See generic: \code{\link[reactr]{bumpGitVersion}}
#'   	 
#' @inheritParams bumpGitVersion
#' @param .ns \code{\link{Bumpr.Git.S3}}.
#' @return \code{\link{character}}. New Git version that the project has 
#'    been bumped to.
#' @example inst/examples/bumpGitVersion.r
#' @seealso \code{
#'    \link[reactr]{bumpGitVersion}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bumpGitVersion", 
  signature = signature(
    .ns = "Bumpr.Git.S3"
  ), 
  definition = function(
    temp_credentials,
    .ns,
    ...
  ) {

  return(bump(what = .ns, temp_credentials = temp_credentials, ...))
    
  }
)
