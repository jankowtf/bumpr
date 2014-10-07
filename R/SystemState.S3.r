#' @title
#' Class: SystemState.S3 
#'
#' @description
#' Class representing the system state (S3) and its constructor function.
#' 
#' @template intended-use
#'
#' @param .x \code{\link{ANY}}. An object of an arbitrary class whose class
#'    attribute should be updated so that it becomes an instance of class
#'    \code{SystemState.S3}. Mainly intended for rapid prototyping 
#'    purposes
#'    
#' @field ask_authentication \code{\link{logical}}.
#'    Controls if user is asked how to handle authentication.
#'    Default: \code{TRUE}.
#' @field branch \code{\link{character}}.
#'    Name of branch to push to remote.
#'    Default: \code{"master"}.
#' @field cmd_user_email \code{\link{character}}.
#'    Git command used to query \code{user.email}.
#'    Default: \code{"git config --global user.email"}.
#' @field cmd_user_name \code{\link{character}}.
#'    Git command used to query \code{user.name}.
#'    Default: \code{"git config --global user.name"}.
#' @field description_old \code{\link{character}}.
#'    Old state of \code{DESCRIPTION} file that can be used to rollback 
#'    changes.
#'    Default: \code{list()}.
#' @field git_tag \code{\link{character}}.
#'    Git tag for new version.
#'    Default: \code{character()}.
#' @field git_user_email \code{\link{character}}.
#'    Git value for \code{user.email}.
#'    Default: \code{character()}.
#' @field git_user_name \code{\link{character}}.
#'    Git value for \code{user.name}.
#'    Default: \code{character}.
#' @field global_or_local \code{\link{character}}.
#'    Controls if global or local Git user credentials are used.
#'    Default: \code{"global"}.
#' @field pat_or_basic \code{\link{logical}}.
#'    Controls if a personal access token (PAT or OAuth token) or basic 
#'    HTTPS authentication should be used.
#'    Default: \code{character()} which means that user is asked.
#' @field path_netrc \code{\link{character}}.
#'    Path to \code{_netrc} file used for HTTPS authentication.
#'    Default: \code{file.path(Sys.getenv("HOME"), "_netrc")}.
#' @field path_netrc_tmp \code{\link{character}}.
#'    Path to temporary \code{_netrc} file used for HTTPS authentication.
#'    Default: \code{file.path(Sys.getenv("HOME"), "_netrc_0")}.
#' @field quit \code{\link{logical}}.
#'    Controls if function quits. 
#'    Default: \code{FALSE}.
#' @field remote \code{\link{character}}.
#'    Name \strong{or} URL of remote repository to push to (depending on 
#'    authentication type).
#'    Default: \code{character()}.
#' @field remote_all \code{\link{character}}.
#'    Name and URL of remote repository to push to.
#'    Default: \code{list(origin = character())}.
#' @field remote_name \code{\link{character}}.
#'    Name of remote repository to push to.
#'    Default: \code{"origin"}.
#' @field remote_url \code{\link{character}}.
#'    URL of remote repository to push to.
#'    Default: \code{character()}.
#' @field temp_credentials \code{\link{logical}}.
#'    Controls if HTTPS credentials should only be stored temporarily.
#'    Only relevant if authentication type is \code{basic} (HTTPS).
#'    Default: \code{FALSE}.
#' @field what \code{\link{ANY}}.
#'    Contains the object to bump. 
#'    Default: \code{NA}.
#' @field quit \code{\link{logical}}.
#'    Controls if function quits.
#'    Default: \code{FALSE}.
#' @return Instance of class \code{SystemState.S3}.
#' @example inst/examples/SystemState.S3.r
#' @seealso \code{
#'   	\link[bumpr]{GitVersion.S3}
#'     \link[bumpr]{RPackageVersion.S3}
#' }
#' @template author
#' @template references
#' @export
SystemState.S3 <- function(
  .x,
  ask_authentication = TRUE,
  branch = "master",
  cmd_user_email = "git config --global user.email",
  cmd_user_name = "git config --global user.name",
  description_old = list(),
  git_tag = character(),
  git_user_email = character(),
  git_user_name = character(),
  global_or_local = "global",
  pat_or_basic = character(),
  path_netrc = file.path(Sys.getenv("HOME"), "_netrc"),
  path_netrc_tmp = file.path(Sys.getenv("HOME"), "_netrc_0"),
  quit = FALSE,
  remote = character(),
  remote_all = list(origin = character()),
  remote_name = "origin",
  remote_url = character(),
  temp_credentials = FALSE,
  what = NA
) {
  if (!missing(.x)) {
    class(.x) <- c("SystemState.S3", class(.x))
    out <- .x
  } else {
    out <- new.env()
    
    out$ask_authentication <- ask_authentication
    out$branch <- branch
    out$cmd_user_email <- cmd_user_email
    out$cmd_user_name <- cmd_user_name
    out$description_old <- description_old
    out$git_tag <- git_tag
    out$git_user_email <- git_user_email
    out$git_user_name <- git_user_name
    out$global_or_local <- global_or_local
    out$pat_or_basic <- pat_or_basic
    out$path_netrc <- path_netrc
    out$path_netrc_tmp <- path_netrc_tmp
    out$quit <- quit
    out$remote <- remote
    out$remote_all <- remote_all
    out$remote_name <- remote_name
    out$remote_url <- remote_url
    out$temp_credentials <- temp_credentials
    out$what <- what
    
    class(out) <- c("SystemState.S3", class(out))
  }
  return(out)
}
