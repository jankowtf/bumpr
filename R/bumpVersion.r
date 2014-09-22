#' @title
#' Bump Version
#'
#' @description 
#' Bumps ressources containing versions to new versions.
#'   	
#' @param ns \strong{Signature argument}.
#'    Object containing namespace information.
#' @param from \strong{Signature argument}.
#'    Object containing current version information.
#' @param to \strong{Signature argument}.
#'    Object containing target version information.
#' @template threedot
#' @example inst/examples/bumpVersion.r
#' @seealso \code{
#'   	\link[reactr]{bumpVersion-character-environment-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "bumpVersion",
  signature = c(
    "from",
    "to",
    "ns"
  ),
  def = function(
    from,
    to,
    ns,
    ...
  ) {
    standardGeneric("bumpVersion")       
  }
)

#' @title
#' Bump Version
#'
#' @description 
#' See generic: \code{\link[reactr]{bumpVersion}}
#'      
#' @inheritParams bumpVersion
#' @param from \code{\link{missing}}.
#' @param to \code{\link{missing}}.
#' @param ns \code{\link{Versionbumpr.Git.S3}}.
#' @return See method
#'    \code{\link[reactr]{bumpVersion-character-character-Versionbumpr.Git.S3-method}}
#' @example inst/examples/bumpVersion.r
#' @seealso \code{
#'    \link[reactr]{bumpVersion}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bumpVersion", 
  signature = signature(
    from = "missing",
    to = "missing",
    ns = "Versionbumpr.Git.S3"
  ), 
  definition = function(
    from,
    to,
    ns,
    ...
  ) {

  ## Read description file //
  tmp <- read.dcf("DESCRIPTION")
  desc <- as.list(tmp)
  names(desc) <- colnames(tmp)  
  
  ## Get version //
  vsn_0 <- desc$Version
  vsn_1 <- unlist(strsplit(vsn_0, split = "\\."))
  vsn_list <- list(major = character(), minor = character(), 
                   patch = character(), dev = character())
  for (ii in seq(along = vsn_1)) {
    vsn_list[[ii]] <- vsn_1[ii]
  }
  vsn_vec <- unlist(vsn_list)
  vsn_sug <- vsn_vec 
  vsn_sug[length(vsn_vec)] <- as.numeric(vsn_sug[length(vsn_vec)]) + 1
  vsn_sug <- paste(vsn_sug, collapse = ".")

  message(paste0("Current version: ", vsn_0))
  message(paste0("Suggested version: ", vsn_sug))

  ## New version //
  input <- readline(paste0("Enter version number [", vsn_sug, "]: "))
  input <- ifelse(grepl("\\D", input), input, NA)
  if (is.na(input)){
    message(paste0("Using suggested version: ", vsn_sug))
    vsn_new <- vsn_sug
  } else {
    res <- ifelse(!grepl("^\\d+\\.\\d+(\\.\\d+){0,2}$", input), NA, input)
    if (is.na(res)) {
      msg <- c(
        paste0("Invalid version number provided: ", input),
        "(expecting {major}.{minor} or any one or both of .{patch} and .{dev}"
      )
      stop(paste(msg, collapse = "\n"))
    }
  }
  vsn_new <- input
  
  message(paste0("Will set new version to be DESCRIPTION file: ", vsn_new))

  ## Update DESCRIPTION FILE
  desc$Version <- vsn_new
  desc$Date <- Sys.time()
  
  ## Write DESCRIPTION FILE
  write.dcf(as.data.frame(desc), file = "test")
  
  ## Git //
  input <- readline(paste0("Ready to commit to git? [yes/no]: "))
  input <- ifelse(grepl("\\D", input), input, NA)
  if (is.na(input)){
    message("Exiting")
    return(TRUE)
  } 
  
  input <- readline(paste0("Git repository to push to: "))
  idx <- ifelse(grepl("\\D", input), input, NA)
  if (is.na(idx)){
    message("Using repository 'origin'")
    git_repos <- "origin"
  } else {
    git_repos <- idx
  }
  
  ## Validate repository //
  git_remote <- system("git remote", intern = TRUE)
  if (!length(git_remote)) {
    stop("No remote git repositories set yet. Specify at least 'origin'")
  }
  if (!git_repos %in% git_remote) {
    msg <- c(
      paste0("Invalid remote git repository specified: ", git_repos),
      paste0("Available remote git repositories: ", paste(git_remote, collapse = ", "))
    )
    stop(paste(msg, collapse="\n"))
  }
  
  ## Checking for initial commit //
  git_log <- suppressWarnings(system("git log", intern = TRUE))
  if (!grepl("fatal: bad default revision HEAD", git_log[1])) {
    has_initial <- FALSE
  } else {
    has_initial <- TRUE
  }
  
  

vsn_0 <- "0.1.0.1"
vsn_new <- "0.1.0.2"
  if (has_initial) {
    ## CHANGES //
    tmpfile <- tempfile()
    if (!file.exists("CHANGES")) {
      write("", file = "CHANGES")
    }
    tmp_new <- c(
      paste0("Version ", vsn_new, ":"),
      system(paste0("git log --pretty=format:\" - %s\" ", "\"v", vsn_0, "\"...HEAD"),
             intern = TRUE),
      "",
      "",
      readLines("CHANGES")
    )
    write(tmp_new, file = tmpfile)
    file.rename(from = tmpfile, to = "CHANGES")
    
    ## Git commands //
    git_commands <- c(
      "git add CHANGES DESCRIPTION",
      paste0("git commit -m 'Version bump to ", vsn_new, "'"),
      paste0("git tag -a -m 'Tagging version ", vsn_new, "' 'v", vsn_new, "'"),
      paste0("git push ", git_repos, " --tags")
    )
  }
  

  return(bumpVersion(from = from, to = to, ns = ns, ...))
    
  }
)

#' @title
#' Get Value from Environment
#'
#' @description 
#' See generic: \code{\link[reactr]{bumpVersion}}
#'   	 
#' @inheritParams bumpVersion
#' @param from \code{\link{character}}.
#' @param to \code{\link{character}}.
#' @param ns \code{\link{Versionbumpr.Git.S3}}.
#' @return \code{\link{ANY}}. Variable value
#' @example inst/examples/bumpVersion.r
#' @seealso \code{
#'    \link[reactr]{bumpVersion}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bumpVersion", 
  signature = signature(
    from = "missing",
    to = "missing",
    ns = "Versionbumpr.Git.S3"
  ), 
  definition = function(
    from,
    to,
    ns,
    ...
  ) {

  ns <- classr::createInstance(cl = "Versionbumpr", 
    obj = list(
      git_repos = "origin"
    )
  )
  ns
  
  
  
  return(out)
    
  }
)
