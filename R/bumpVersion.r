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
    
  ## HOME //
  sys_home <- Sys.getenv("HOME")
  if (sys_home == "") {
    Sys.setenv("HOME" = ns$home)
    sys_home <- Sys.getenv("HOME")
  }
  if (!file.exists(file.path(sys_home, "_netrc"))) {
    msg <- c(
      "Missing file '_netrc' with HTTPS credentials",
      paste0("Create that file in: ", sys_home),
      "Make sure it contains the following content:",
      "  machine github.com",
      "  login <your-username>",
      "  password <your-password>",
      "  protocol https"
    )
    stop(paste(msg, collapse = "\n"))
  }
  
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
    vsn_new <- input
  }
  
  message(paste0("Will set new version to be DESCRIPTION file: ", vsn_new))

  ## Update DESCRIPTION FILE
  desc$Version <- vsn_new
  desc$Date <- Sys.time()
  
  ## Write DESCRIPTION FILE
  write.dcf(as.data.frame(desc), file = "DESCRIPTION")
  
  ## Git //
  input <- readline(paste0("Ready to commit to git? [yes/no]: "))
  input <- ifelse(grepl("\\D", input), input, NA)
  if (is.na(input)){
    message("Exiting")
    return(TRUE)
  } 
  
  input <- readline(paste0("Git repository to push to (default: 'origin'): "))
  idx <- ifelse(grepl("\\D", input), input, NA)
  if (is.na(idx)){
    git_repos <- "origin"
  } else {
    git_repos <- idx
  }
  
  message("Using remote git repository 'origin'")
  
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
  
  ## Git credentials
  user_email <- suppressWarnings(system("git config --global user.email", intern = TRUE))
  if (!length(user_email)) {
    if (!length(ns$user_email)) {
      stop("Empty field in 'ns': user_email")
    }
    system(paste0("git config user.email \"", ns$user_email, "\""))
  }
  user_name <- suppressWarnings(system("git config --global user.name", intern = TRUE))
  if (!length(user_name)) {
    if (!length(ns$user_name)) {
      stop("Empty field in 'ns': user_name")
    }
    system(paste0("git config user.name \"", ns$user_name, "\""))
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
      paste0("git commit -m \"Version bump to ", vsn_new, "\""),
      paste0("git tag -a -m \"Tagging version ", vsn_new, "\" \"v", vsn_new, "\""),
      paste0("git push ", git_repos, " --tags")
    )
    system(git_commands[4])
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

  ns <- classr::createInstance(cl = "Bumpr.Git.S3", 
    obj = list(
      git_repos = "origin",
      user_email = character(),
      user_name = character(),
      home = Sys.getenv("HOME")
    )
  )
  ns$user_email <- "janko.thyson@rappster.de"
  ns$user_name <- "Janko Thyson"
  
  
  
  return(out)
    
  }
)
