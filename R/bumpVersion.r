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
    
  ## Class instance //    
  ns <- classr::createInstance(cl = "Bumpr.Git.S3", 
    obj = list(
      version = character(),
      git_repos = "origin",
      user_email = character(),
      user_name = character(),
      home = Sys.getenv("HOME")
    )
  )
  ns$user_email <- "janko.thyson@rappster.de"
  ns$user_name <- "Janko Thyson"  
    
  ## From and to //
  from <- unname(read.dcf("DESCRIPTION", field = "Version")[1,1])
  if (is.na(from)) {
    stop("Invalid version in DESCRIPTION")
  }
  to <- ns$version    
    
  return(bumpVersion(
    from = from, 
    to = to, 
    ns = ns, 
    ...
  ))
    
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

  ##----------------------------------------------------------------------------
  ## DESCRIPTION file and versions //
  ##----------------------------------------------------------------------------
  
  ## Read description file //
  tmp <- read.dcf("DESCRIPTION")
  desc <- as.list(tmp)
  names(desc) <- colnames(tmp)  
  
  ## Ensure they are the same //
  if (from != desc$Version) {
    from <- desc$Version
  }
  
  ## Get version //
  vsn_0 <- from
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
  input <- readline(paste0("Enter a valid version number [", vsn_sug, "]: "))
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
  
  ## Verify update of DESCRIPTION file //
  input <- readline(paste0("Updating version in DESCRIPTION file to: '", 
    vsn_new, "?' [yes/no]: "))
  input <- ifelse(grepl("\\D", input), tolower(input), "no")
  if (input == "no") {
    message("Exiting")
    return(FALSE)
  }

  ## Update DESCRIPTION FILE
  desc$Version <- vsn_new
  desc$Date <- Sys.time()
  
  ## Write DESCRIPTION FILE
  write.dcf(as.data.frame(desc), file = "DESCRIPTION")
  
  ##----------------------------------------------------------------------------
  ## Git //
  ##----------------------------------------------------------------------------
  
  input <- readline(paste0("Ready to commit to git? [yes/no]: "))
  input <- ifelse(grepl("\\D", input), input, NA)
  if (is.na(input)){
    message("Exiting")
    return(TRUE)
  } else {
    if (input == "no") {
      message("Exiting")
      return(FALSE)
    }
  }
  
  ## Remote repository //
  input <- readline(paste0("Remote git repository (Hit [ENTER] to keep the default: 'origin'): "))
  idx <- ifelse(grepl("\\D", input), input, NA)
  if (is.na(idx)){
    git_repos <- "origin"
  } else {
    git_repos <- idx
  }
  message(paste0("Using remote git repository: ", git_repos))
  
  ## Validate repository //
  git_remote <- system("git remote", intern = TRUE)
  if (!length(git_remote)) {
    stop("No remote git repositories set yet. Specify at least 'origin'")
  }
  if (!git_repos %in% git_remote) {
    msg <- c(
      paste0("Invalid remote git repository specified: ", git_repos),
      paste0("Available remote git repositories: ", paste(git_remote, collapse = ", ")),
      "Exiting"
    )
    message(paste(msg, collapse="\n"))
    return(FALSE)
  }
  
  ## Git user credentials //
  git_user_email <- suppressWarnings(system("git config user.email", intern = TRUE))
  if (!length(git_user_email)) {
    if (!length(ns$user_email)) {
      input <- readline("git user.email: ")
      idx <- ifelse(grepl("\\D", input), input, NA)
      if (is.na(idx)) {
        message("Invalid 'user.email': ", input)
        message("Exiting")
        return(FALSE)
      }
      git_user_email <- input 
    }
    system(paste0("git config user.email \"", ns$user_email, "\""))  
  }
  git_user_name <- suppressWarnings(system("git config user.name", intern = TRUE))
  if (!length(git_user_name)) {
    if (!length(ns$user_name)) {
      input <- readline("git user.name: ")
      idx <- ifelse(grepl("\\D", input), input, NA)
      if (is.na(idx)) {
        message("Invalid 'user.name': ", input)
        message("Exiting")
        return(FALSE)
      }
      git_user_name <- input 
    }
    system(paste0("git config user.name \"", ns$user_name, "\""))  
  }
  
  ## Checking for initial commit //
  git_log <- suppressWarnings(system("git log", intern = TRUE))
  if (!grepl("fatal: bad default revision HEAD", git_log[1])) {
    has_initial <- FALSE
  } else {
    has_initial <- TRUE
  }
  
  if (!has_initial) {
    message(paste0("No initial commit yet."))
    return(FALSE)
  } else {
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
    
    ## HTTPS credentials //
    sys_home <- Sys.getenv("HOME")
    if (sys_home == "") {
      Sys.setenv("HOME" = ns$home)
      sys_home <- Sys.getenv("HOME")
    }
    path_netrc <- file.path(sys_home, "_netrc")
    path_netrc_tmp <- gsub("_netrc", "_netrc_0", path_netrc)
    message(paste0("Using credentials as stored in ", path_netrc, "? (yes/no):"))
    input <- scan(what = "character", n = 1, quiet = TRUE)
    if (length(input)) {
      if (!input %in% c("yes", "no")) {
        message("Invalid decision")
        message("Exiting")
        return(FALSE)
      }
      use_stored_creds <- switch(input, "yes" = TRUE, "no" = FALSE)
    } else {
      use_stored_creds <- FALSE
    }
    
    if (use_stored_creds) {
      if (!file.exists(path_netrc)) {
        msg <- c(
          "Missing file '_netrc' with HTTPS credentials",
          paste0("Create that file in: ", sys_home),
          "Make sure it contains the following content:",
          "  machine github.com",
          "  login <your-username>",
          "  password <your-password>",
          "  protocol https",
          "Exiting"
        )
        message(paste(msg, collapse = "\n"))
        return(FALSE)
      }
    } else {
      message("Username for 'https://github.com': ")
      input <- scan(what = "character", n = 1, quiet = TRUE)
      if (!length(input)) {
        message("Empty username")
        message("Exiting")
        return(FALSE)
      }
      git_https_username <- input
    
      message(paste0("Password for 'https://", git_user_email, "@github.com': "))
      input <- scan(what = "character", n = 1, quiet = TRUE)
      if (!length(input)) {
        message("Empty password")
        message("Exiting")
        return(FALSE)
      }
      git_https_password <- input
      cnt <- c(
        "machine github.com",
        paste0("login ", git_https_username),
        paste0("password ", git_https_password),
        "protocol https"
      )
      if (file.exists(path_netrc)) {
        file.rename(from = path_netrc, to = path_netrc_tmp)
      }
      write(cnt, file = path_netrc)
    }
      
    ## Git commands //
    git_commands <- c(
      "git add CHANGES DESCRIPTION",
      paste0("git commit -m \"Version bump to ", vsn_new, "\""),
      paste0("git tag -a -m \"Tagging version ", vsn_new, "\" \"v", vsn_new, "\""),
      paste0("git push ", git_repos, " --tags")
    )
    
    res <- tryCatch(
      sapply(git_commands, function(cmd) {
        system(cmd, intern = TRUE)
      }),
      error = function(cond) {
        stop(cond)
      },
      finally = {
        if (file.exists(path_netrc_tmp)) {
          file.rename(from = path_netrc_tmp, to = path_netrc)
        }
      }
    )
  }  
    
  return(out)
    
  }
)
