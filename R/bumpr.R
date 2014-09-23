#' @title
#' Easy systematic version bumping and more
#'
#' @description
#' Makes it really easy to bump the version of an R package project placed 
#' under Git version control (see \url{http://git-scm.com}) to a new version 
#' number. 
#' 
#' @details 
#' Version numbers must follow the \emph{semantical versioning} conventions
#' (see \url{http://semver.org/}).
#' 
#' The core functions/methods of this package: 
#'  \itemize{
#'    \item{\code{\link[bumpr]{bumpGitVersion}}: }{
#'    
#'      Performs all sorts of Git-related checks and tasks in order to take care
#'      that everything necessary is done that is related to bumping a project
#'      to a higher version number. 
#'      This also implies that the \emph{package version} (field \code{Version})
#'      and the \emph{package date} (field \code{Date}) in the 
#'      \code{DESCRIPTION} file are updated accordingly. 
#'      
#'      Changes are tracked systematically in the file \code{CHANGES.md} 
#'      which is modified at each version bump.
#'      
#'      Essentially, this function is a mere convenience wrapper for the 
#'      actual workhorse function \code{\link[bumpr]{bump}} and its methods.
#'    }
#'    \item{\code{\link[bumpr]{bump}}: }{
#'    
#'      The actual workhorse function of this package. Introduced in order to 
#'      generalize the package's purpose and scope to other aspects than 
#'      version bumping. 
#'      
#'      The long-term goal of this package is to provide a simple interface
#'      to all sorts of things "that can be bumped".
#'    }
#' }
#' 
#' @section Disclaimer:
#' \itemize{
#'    \item{
#'    \strong{This package is really new. So please test it with 
#'    repositories that are not crucial for your productive work!}
#'    }
#' }
#' 
#' @section Inspiration and aknowledgements:
#' This package was greatly inspired by these ressources:
#' \itemize{
#'    \item{\url{http://nvie.com/posts/a-successful-git-branching-model/}: } {
#'      A very thorough and well designed branching model.
#'    }
#'    \item{\url{https://gist.githubusercontent.com/pete-otaqui/4188238/raw/9675e75948294fd3b05186f30af6ff97e7e9e47e/bumpversion.sh}: } {
#'      Bash script for Git version bumping.
#'    }
#'    \item{\url{http://stackoverflow.com/questions/18648737/use-r-to-push-local-repo-to-github-on-windows}: } {
#'      Description of how to access GitHub repositories via R using HTTP
#'      authentication.
#'    }
#'    
#' }
#' 
#' @template author
#' @template references
#' @docType package
#' @name bumpr
#' @seealso \url{http://git-scm.com}, \url{https://github.com/}
NULL
