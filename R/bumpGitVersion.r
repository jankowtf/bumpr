#' @title
#' Bump Git Version
#'
#' @description 
#' Bumps the version of a git project to the next version.
#' 
#' @details
#' Asumptions:
#' \itemize{
#'   \item{Git project: }{
#'      Assumes that a valid Git project has been created 
#'      (look for directory \code{.git})
#'   }
#'   \item{Git project: }{
#'      Assumes that an initial commit has already been issued.
#'      (Run \code{git log} for your commit history. If this fails, you haven't 
#'      done an intial commit yet)
#'   }
#'   \item{Git remote repository: }{
#'      Assumes that at least the remote repository with name \code{origin}
#'      has been set. 
#'      (Run \code{git remote} to find out about your remote repositories)
#'   }
#'   \item{GitHub HTTP credentials: }{
#'      If you want to push to a GitHub repository, the function assumes that 
#'      you are willing to store your HTTP credentials for 
#'      \url{https://github.com} (\code{username} and \code{password} in file 
#'      \code{file.path(Sys.getenv("HOME)/_netrc)} as this is where Git expects
#'      to find HTTP credentials when pushing. 
#'      You can choose to destroy this file after each bump by setting
#'      \code{temp_credentials = TRUE}.
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
#' Bump Git Version
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
#' Bump Git Version
#'
#' @description 
#' See generic: \code{\link[reactr]{bumpGitVersion}}
#'   	 
#' @inheritParams bumpGitVersion
#' @param .ns \code{\link{Bumpr.Git.S3}}.
#' @return \code{\link{character}}. Git version the project has been bumped to.
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
