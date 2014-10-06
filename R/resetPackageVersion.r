#' @title
#' Reset Package Version Number
#'
#' @description 
#' Resets the version number of R package project to a desired version number.
#' Typically, this function is only called if something went wrong in either
#' \code{\link[bumpr]{bumpPackageVersion}} or 
#' \code{\link[bumpr]{bumpGitVersion}}.
#' 
#' @param vsn \strong{Signature argument}.
#'    Object containing version information.
#' @param desc_fields \code{\link{list}}.
#'    Additional fields (besides \code{Version}) in DESCRIPTION file that 
#'    should also be reset. Specified as name-value pairs.
#' @template threedot
#' @example inst/examples/resetPackageVersion.r
#' @seealso \code{
#'   	\link[bumpr]{resetPackageVersion-GitVersion.S3-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "resetPackageVersion",
  signature = c(
    "vsn"
  ),
  def = function(
    vsn,
    desc_fields = list(),
    ...
  ) {
    standardGeneric("resetPackageVersion")       
  }
)

#' @title
#' Reset Package Version Number
#'
#' @description 
#' See generic: \code{\link[bumpr]{resetPackageVersion}}
#'      
#' @inheritParams resetPackageVersion
#' @param vsn \code{\link{character}}.
#' @return \code{\link{packageDescription}}.
#' @example inst/examples/resetPackageVersion.r
#' @seealso \code{
#'    \link[bumpr]{resetPackageVersion}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "resetPackageVersion", 
  signature = signature(
    vsn = "character"
  ), 
  definition = function(
    vsn,
    desc_fields,
    ...
  ) {
    
  ## Validate //
  if (!file.exists("DESCRIPTION")) {
    stop("Not a valid R package project. Consult 'devtools' package")
  }
  ## Read description file //
  desc <- as.list(read.dcf("DESCRIPTION")[1,])
  
  if (length(desc_fields)) {
    invalid_fields <- setdiff(names(desc_fields), names(desc))
    if (length(invalid_fields)) {
      msg <- c(
        "Invalid additional DESCRIPTION fields specified:",
        paste(invalid_fields, collapse = ", ")
      )
      stop(paste(msg, collapse="\n"))
    }
  }
  
  ## Update DESCRIPTION file //
  desc$Version <- vsn
  for (ii in names(desc_fields)) {
    desc[[ii]] <- desc_fields[[ii]]
  }

  ## Write DESCRIPTION FILE
  write.dcf(as.data.frame(desc), file = "DESCRIPTION")  
  
  }
)

