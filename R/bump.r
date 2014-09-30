#' @title
#' Bump Something
#'
#' @description 
#' Bumps a ressource - whatever that means is left up to the methods of this
#' generic function.
#'   	
#' @param what \strong{Signature argument}.
#'    Object containing information about what should be bumped.
#' @param from \strong{Signature argument}.
#'    Object containing current information.
#' @param to \strong{Signature argument}.
#'    Object containing new information.
#' @template threedot
#' @example inst/examples/bump.r
#' @seealso \code{
#'   	\link[reactr]{bump-Bumpr.GitVersion.S3-character-character-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "bump",
  signature = c(
    "what",
    "from",
    "to"
  ),
  def = function(
    what,
    from,
    to,
    ...
  ) {
    standardGeneric("bump")       
  }
)

#' @title
#' Bump Package Version Number
#'
#' @description 
#' See generic: \code{\link[reactr]{bump}}
#'      
#' @inheritParams bump
#' @param what \code{\link{Bumpr.RPackageVersion.S3}}.
#' @param from \code{\link{missing}}.
#' @param to \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{bump-character-character-Bumpr.RPackageVersion.S3-method}}
#' @example inst/examples/bump.r
#' @seealso \code{
#'    \link[reactr]{bump}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bump", 
  signature = signature(
    what = "Bumpr.RPackageVersion.S3",
    from = "missing",
    to = "missing"
  ), 
  definition = function(
    what,
    from,
    to,
    temp_credentials = FALSE,
    ...
  ) {
    
  ## From and to //
  if (!file.exists("DESCRIPTION")) {
    stop("Not a valid R package project. Consult 'devtools' package")
  }    
  from <- unname(read.dcf("DESCRIPTION", field = "Version")[1,1])
  if (is.na(from)) {
    stop("Invalid version in DESCRIPTION")
  }
  to <- as.character(what$version)
    
  return(bump(
    what = what,
    from = from, 
    to = to, 
    temp_credentials,
    ...
  ))
    
  }
)

#' @title
#' Bump Package Version Number
#'
#' @description 
#' See generic: \code{\link[reactr]{bump}}
#'      
#' @inheritParams bump
#' @param what \code{\link{Bumpr.RPackageVersion.S3}}.
#' @param from \code{\link{character}}.
#' @param to \code{\link{character}}.
#' @param taken \code{\link{character}}.
#'    Version numbers that are already taken. Usually, these are the Git 
#'    tags that correspond to release versions. If used in combination 
#'    with \code{\link{bump-Bumpr-Git.S3-character-character-method}}, this
#'    information is automatically retrieved.
#' @param desc_fields \code{\link{list}}.
#'    Additional fields (besides \code{Version}) in DESCRIPTION file that 
#'    should be updated with a version number bump. Specified as name-value pairs.
#' @return \code{\link{list}}. (\code{old} and \code{new} package version or 
#'    \code{list()} if the function exited before completing the bump.
#' @example inst/examples/bump.r
#' @seealso \code{
#'    \link[reactr]{bump}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bump", 
  signature = signature(
    what = "Bumpr.RPackageVersion.S3",
    from = "character",
    to = "character"
  ), 
  definition = function(
    what,
    from,
    to,
    taken = character(),
    desc_fields = list("Date" = NULL),
    ...
  ) {
    
  ## Private functions //
  .processUserInput <- function(input, dflt = "yes") {
    input <- ifelse(grepl("\\D", input), tolower(input), dflt)
    if (grepl("[qQ]|Quit|quit|QUIT", input)) {
      out <- NULL
    } else if (grepl("[nN]|No|no|NO", input)) {
      out <- FALSE
    } else if (grepl("[yY]|Yes|yes|YES", input)) {
      out <- TRUE
    } else {
      message(paste0("Invalid input: ", input))
      out <- NULL
    }
  }   
  .askUpdateDescriptionFile <- function(force = logical(), vsn) {
    if (!length(force)) {
      input <- readline(paste0("Updating version in DESCRIPTION file to '", 
        vsn, "?' [(y)es | (n)o | (q)uit]: "))
      out <- .processUserInput(input = input, dflt = "yes")
    } else {
      out <- force
    }
    return(out)
  }    
  .askNewVersionNumber <- function(taken) {
    input <- readline(paste0("Enter a valid version number: [ENTER = ", vsn_sug, "] "))
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
    ## Check against taken version numbers //
    while (vsn_new %in% taken) {
      message(paste0("Version number '", vsn_new, "' already taken"))
      message("Taken version numbers (last 10):")
      if (length(taken) > 10) {
        message(paste(taken[(length(taken) - 10):length(taken)], collapse="\n"))
      } else {
        message(paste(taken, collapse="\n"))
      }
      message("Choose another version number")
      vsn_new <- .askNewVersionNumber(taken = taken)
    }
    vsn_new
  }

  ## Validate //
  if (!file.exists("DESCRIPTION")) {
    stop("Not a valid R package project. Consult 'devtools' package")
  }
  if (length(desc_fields)) {
    invalid_fields <- setdiff(names(desc_fields), c("Date"))
    if (length(invalid_fields)) {
      msg <- c(
        "Invalid additional DESCRIPTION fields specified:",
        paste(invalid_fields, collapse = ", ")
      )
      stop(paste(msg, collapse="\n"))
    }
  }
  
  ## Rename //
  vsn_old <- from
  vsn_new <- to
  
  ## Read description file //
  tmp <- read.dcf("DESCRIPTION")
  desc <- as.list(tmp)
  names(desc) <- colnames(tmp)  
  
  ## Taken versions //
  if (length(taken)) {
    message("Taken versions numbers (last 10): ")
    tmp <- sort(numeric_version(gsub("^v(?=\\d)", "", taken, perl = TRUE)))
    if (length(tmp) > 10) {
      tmp <- tmp[(length(tmp) - 10):length(tmp)]
    }
    message(paste0(tmp, collapse="\n"))
  }
  
  ## Ensure correct current version number //
  if (!length(vsn_old)) {
    vsn_old <- desc$Version
  } else if (from != desc$Version) {
    vsn_old <- desc$Version
  }
  message(paste0("Current version: ", vsn_old))
  
  if (!length(vsn_new)) {
    ## Get suggested version //
    vsn_1 <- unlist(strsplit(vsn_old, split = "\\."))
    vsn_list <- list(major = character(), minor = character(), 
                     patch = character(), dev = character())
    for (ii in seq(along = vsn_1)) {
      vsn_list[[ii]] <- vsn_1[ii]
    }
    vsn_vec <- unlist(vsn_list)
    vsn_sug <- vsn_vec 
    vsn_sug[length(vsn_vec)] <- as.numeric(vsn_sug[length(vsn_vec)]) + 1
    vsn_sug <- paste(vsn_sug, collapse = ".")

    message(paste0("Suggested version: ", vsn_sug))

    ## New version //
    vsn_new <- .askNewVersionNumber(taken = taken)
  }
  
  ## Verify update of DESCRIPTION file //  
  if (vsn_new != vsn_old) {
    res <- .askUpdateDescriptionFile(vsn = vsn_new)
    if (is.null(res) || !res) {
      message("Quitting")
      return(list())
    }
    ## Update DESCRIPTION file //
    desc$Version <- vsn_new
    if ("Date" %in% names(desc_fields)) {
      desc$Date <- Sys.time()
    }
    
    ## Write DESCRIPTION FILE
    write.dcf(as.data.frame(desc), file = "DESCRIPTION")  
  }

  return(list(old = vsn_old, new = vsn_new))
    
  }
)

#' @title
#' Bump Git Version Number
#'
#' @description 
#' See generic: \code{\link[reactr]{bump}}
#'      
#' @inheritParams bump
#' @param what \code{\link{Bumpr.GitVersion.S3}}.
#' @param from \code{\link{missing}}.
#' @param to \code{\link{missing}}.
#' @param temp_credentials \code{\link{logical}}.
#'    \code{TRUE}: delete HTTPS credentials after each bump;
#'    \code{FALSE}: permanently store HTTPS credentials in \code{_netrc} file.
#'    See details.   
#' @param pat \code{\link{logical}}.
#'    \code{TRUE}: use a personal authentication token as stored in the system
#'    environment variable \code{GITHUB_PAT};
#'    \code{FALSE}: rely on basic HTTPS authentication (username and password
#'    including the use of a (temporary) credentials file \code{_netrc}).
#' @return See method
#'    \code{\link[reactr]{bump-character-character-Bumpr.GitVersion.S3-method}}
#' @example inst/examples/bump.r
#' @seealso \code{
#'    \link[reactr]{bump}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bump", 
  signature = signature(
    what = "Bumpr.GitVersion.S3",
    from = "missing",
    to = "missing"
  ), 
  definition = function(
    what,
    from,
    to,
    project = character(),
    temp_credentials = FALSE,
    pat = TRUE,
    ...
  ) {
    
  ## From and to //
  from <- unname(read.dcf("DESCRIPTION", field = "Version")[1,1])
  if (is.na(from)) {
    stop("Invalid version in DESCRIPTION")
  }
  to <- as.character(what$version)
    
  return(bump(
    what = what,
    from = from, 
    to = to, 
    project = project,
    temp_credentials = temp_credentials,
    pat = pat,
    ...
  ))
    
  }
)

#' @title
#' Bump Git Version Number
#'
#' @description 
#' See generic: \code{\link[reactr]{bump}}
#'   	 
#' @inheritParams bump
#' @param what \code{\link{Bumpr.GitVersion.S3}}.
#' @param from \code{\link{character}}.
#' @param to \code{\link{character}}.
#' @return \code{\link{list}}. (\code{old} and \code{new} package version or 
#'    \code{list()} if the function exited before completing the bump.
#' @example inst/examples/bump.r
#' @seealso \code{
#'    \link[reactr]{bump}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bump", 
  signature = signature(
    what = "Bumpr.GitVersion.S3",
    from = "character",
    to = "character"
  ), 
  definition = function(
    what,
    from,
    to,
    project,
    temp_credentials,
    pat,
    ...
  ) {
    
  ## Private functions //
  .processUserInput <- function(input, dflt = "yes") {
    input <- ifelse(grepl("\\D", input), tolower(input), dflt)
    if (grepl("[qQ]|Quit|quit|QUIT", input)) {
      out <- NULL
    } else if (grepl("[nN]|No|no|NO", input)) {
      out <- FALSE
    } else if (grepl("[yY]|Yes|yes|YES", input)) {
      out <- TRUE
    } else {
      message(paste0("Invalid input: ", input))
      out <- NULL
    }
  }    
    
  .ask_gitAddGitignoreFile <- function(force = logical()) {
    if (!length(force)) {
      input <- readline("Add a '.gitignore' file (highly recommended)? [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes")
    } else {
      out <- force
    }
    return(out)
  }  
  .ask_gitDoInitialCommit <- function(force = logical()) {
    if (!length(force)) {
      input <- readline("Perform initial commit (or quit)? [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes")
    } else {
      out <- force
    }
    return(out)
  }  
  .ask_gitSetRemote <- function(force = logical()) {
    if (!length(force)) {
      input <- readline("Would you like to add a remote repository now? [(y)es | (n)o | (q)uit ]: ")
      out <- .processUserInput(input = input, dflt = "yes")
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitPushToRemote <- function(name = "origin", force = logical()) {
    if (!length(force)) {
      input <- readline(paste0("Would you like to push to ", name, 
                               "? [(y)es | (n)o | (q)uit ]: "))
      out <- .processUserInput(input = input, dflt = "yes")
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitRemoteName <- function(force = character()) {
    if (!length(force)) {
      input <- readline("Name of remote git repository (hit ENTER for default = 'origin'): ")
      input <- ifelse(grepl("\\D", input), input, "origin")
      if (input == "") {
        message("Invalid input")
        out <- NULL
      } else {
        out <- input
      } 
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitChangeOrSetRemote <- function(force = logical()) {
    if (!length(force)) {
      input <- readline("Change remote name or set? [(c)hange | (s)et | (q)uit ]: ")
      input <- ifelse(grepl("\\D", input), input, "change")
      if (grepl("[qQ]|Quit|quit|QUIT", input)) {
        out <- NULL
      } else if (grepl("[cC]|Change|change|CHANGE", input)) {
        out <- "change"
      } else if (grepl("[sS]|set|Set|SET", input)) {
        out <- "set"
      } else {
        message(paste0("Invalid input: ", input))
        out <- NULL
      } 
    } else {
      out <- force
    }
    return(out)
  }

  .ask_gitReadyToBumpVersion <- function(force = logical(), vsn) {
    if (!length(force)) {
      input <- readline("Ready to bump version in git?' [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes")
    } else {
      out <- force
    }
    return(out)
  }
  
  .ask_gitValidatedRemoteName <- function(
    git_remote
  ) {
    git_repos <- .ask_gitRemoteName()
    message(paste0("Using remote git repository: ", git_repos))
  
    if (!length(git_remote) || !git_repos %in% git_remote) {
      message("No such remote repository: ", git_repos)
      message(paste0("Available remote repositories: ", 
                     paste(git_remote, collapse = ", ")))
    
      remote_dec <- .ask_gitChangeOrSetRemote()
      if (is.null(remote_dec)) {
        return(NULL)
      } else if (remote_dec == "change") {
        git_repos <- .ask_gitValidatedRemoteName(git_remote = git_remote)
      } else if (remote_dec == "set") {
        .gitSetRemote(name = git_repos)
        push_to_remote <- .ask_gitPushToRemote(name = git_repos)
        if (is.null(push_to_remote)) {
          return(NULL)
        }
        if (push_to_remote) {
          res <- .gitPushToRemote(to = git_repos)
          if (is.null(res)) {
            return(NULL)
          }
        }
      }
    }
    return(git_repos)
  }
  .ask_useStoredCredentials <- function(force = logical()) {
    if (!length(force)) {
      input <- readline("Use stored HTTPS credentials (no = type them)? [(y)es | (n)o | (q)uit ]: ")
      out <- .processUserInput(input = input, dflt = "yes") 
    } else {
      out <- force
    }
    return(out)
  }
  
  ##----------
  
  .gitAddGitignoreFile <- function() {
    message("Will create and open a 'gitignore' file with default content")
    input <- readline("Modify, save and close afterwards [hit ENTER now]: ")
    gitignore_dflt <- c(
    "packrat/lib*/",
      "packrat/src",
      "*.dll",
      "*.Rhistory",
      "*.Rproj.user",
      ".*so",
      ".*o"
    )
    tmpfile <- tempfile()
    write(gitignore_dflt, file = tmpfile)
    message("Choose a suitable text editor to open the file with")
    message(paste0("Otherwise open manually: ", tmpfile))
    if (.Platform['OS.type'] == "windows"){
      shell.exec(tmpfile)
    } else {
      system(paste(Sys.getenv("R_BROWSER"), tmpfile))
    }
    readline("Hit ENTER when you are done: ")
    file.rename(from = tmpfile, to = ".gitignore")
  }
  .gitDoInitialCommit <- function(check_gitignore = FALSE) {
    if (!file.exists(".gitignore") && check_gitignore) {
      add_gitignore <- .ask_gitAddGitignoreFile()
      if (is.null(add_gitignore)) {
        message("Quiting")
        return(NULL)
      }
      if (add_gitignore) {
        .gitAddGitignoreFile()
      }
    }
    res <- system("git add --all", intern = TRUE)
    res <- system("git commit -m \"Initial commit\"", intern = TRUE)
    res <- suppressWarnings(system("git log", intern = TRUE))
    if (grepl("fatal: bad default revision 'HEAD'", res[1])) {
      message("Something went wrong with initial commit")
      return(NULL)
    }  
    res
  }
  
  .gitIsRemoteRepository <- function(name = "origin") {
    res <- suppressWarnings(system(paste0("git ls-remote ", name),
                                   intern = TRUE))
    if (grepl("fatal:.*does not appear to be a git repository", res[1])) {
      out <- FALSE
    } else {
      out <- TRUE
    }
    return(out)
  }
#   .gitIsRemoteRepository()
  .gitIsLocalRepository <- function() {
    res <- suppressWarnings(system("git log --oneline", intern = TRUE))
    if (grepl("fatal:.*Not a git repository", res[1])) {
      out <- FALSE
    } else {
      out <- TRUE
    }
    return(out)
  }
#  .gitIsLocalRepository()
  .gitSetRemote <- function(
    name = "origin",
    url = character()
  ) {
    if (!length(name)) {
      input <- readline("Repository name [ENTER='origin']: ")
      name <- ifelse(grepl("\\D", input), input, "origin")
    } 
    if (!length(url)) {
      input <- readline("Repository URL: ")
      input <- ifelse(grepl("\\D", input), input, NA)
      if (is.na(input)) {
        message("Invalid input")
        message("Quitting")
        return(list())
      }
      url <- paste0("\"", input, "\"")
    }
    msg <- system(paste0("git remote add ", name, " ", url), intern = TRUE)
    message(msg)
    msg
  }
  .gitPushToRemote <- function(from = "master", to = "origin") {
    if (!.gitIsRemoteRepository(name = to)) {
      message(paste0("Not a git remote repository: ", to))
      return(NULL)
    }
    system(paste0("git push ", to, " ", from), intern = TRUE)
    res <- system(paste0("git ls-remote --heads ", to), intern = TRUE)
  }    
  .gitCheckForBranchesInRemote <- function(name = "origin") {
    res <- system(paste0("git ls-remote --heads ", name), intern = TRUE)
    if (!length(res) || grepl("bad default revision", res)) {
      message(paste0("No branches yet in remote repository: ", name))
      push_to_remote <- .ask_gitPushToRemote(name = name)
      if (is.null(push_to_remote)) {
        message("Ensure you pushed at least once to the remote repository")
        return(NULL)
      }
      if (push_to_remote) {
        .gitPushToRemote(to = name)
      }
    }
    res
  }

  ##----------------------------------------------------------------------------
  ## Essential git stuff //
  ##----------------------------------------------------------------------------
  
  if (!.gitIsLocalRepository()) {
    res <- system("git init", intern = TRUE)
  }
  res <- suppressWarnings(system("git log", intern = TRUE))
  if (grepl("fatal: bad default revision 'HEAD'", res[1])) {
    has_initial_commit <- FALSE
  } else {
    has_initial_commit <- TRUE
  }
  if (!has_initial_commit) {
    message("No initial commit yet")
    do_initial_commit <- .ask_gitDoInitialCommit()
    if (is.null(do_initial_commit) || !do_initial_commit) {
      message("You should take care of an initial commit")
      return(list())
    }
    
    ## Gitignore file //
    if (!file.exists(".gitignore")) {
      add_gitignore <- .ask_gitAddGitignoreFile()
      if (is.null(add_gitignore)) {
        message("Quiting")
        return(list())
      }
      if (add_gitignore) {
        .gitAddGitignoreFile()
      }
    }
    
    ## Actual commit //
    res <- .gitDoInitialCommit()
    if (is.null(res)) {
      message("Quiting")
      return(list())
    }
  }

  ## Validate repository //
  git_remote <- system("git remote", intern = TRUE)
  if (!length(git_remote)) {
    message("No remote git repositories set yet. Specify at least 'origin'")
    
    res <- .ask_gitSetRemote()
    if (is.null(res)) {
      message("Make sure you set a remote repository in your local repository")
      message("Quiting")
      return(list())
    }
    .gitSetRemote()
  } 

  ##----------------------------------------------------------------------------
  ## DESCRIPTION file and versions //
  ##----------------------------------------------------------------------------
  
  git_tags <- system("git tag", intern = TRUE)
  res <- bumpPackageVersion(taken = git_tags)
  if (!length(res)) {
    message("Quitting")
    return(list())
  }
  vsn_new <- res$new
  vsn_old <- res$old

  idx <- which(grepl("^v\\d.*", git_tags))
  if (length(idx)) {
    git_vsns <- gsub("^v(?=\\d)", "",git_tags[idx], perl = TRUE)
    if (!vsn_old %in% git_vsns) {
      vsn_old <- max(sort(numeric_version(git_vsns)))
    }
  }

  ##----------------------------------------------------------------------------
  ## Git //
  ##----------------------------------------------------------------------------
  
  res <- .ask_gitReadyToBumpVersion()
  if (is.null(res)) {
    message("Quitting")
    return(list())
  }
  
  ## Validated remote repository //
  git_remote <- system("git remote", intern = TRUE)
  git_repos <- .ask_gitValidatedRemoteName(git_remote = git_remote)
  if (is.null(git_repos)) {
    message("Quitting")
    return(list())
  }

  if (!.gitIsRemoteRepository(name = git_repos)) {
    if(is.null(.gitCheckForBranchesInRemote(name = git_repos))) {
      message("Quitting")
      return(list())
    }
    message(paste0("Not a git remote repository: ", git_repos))
    message("Make sure you have initialized either a bare repository or cloned an existing one")
    message("Quitting")
    return(list())
  }
  if(is.null(.gitCheckForBranchesInRemote(name = git_repos))) {
    message("Quitting")
    return(list())
  }

  ## Git user credentials //
  git_user_email <- suppressWarnings(system("git config user.email", intern = TRUE))
  if (!length(git_user_email)) {
    if (!length(what$user_email)) {
      input <- readline("git user.email: ")
      idx <- ifelse(grepl("\\D", input), input, NA)
      if (is.na(idx)) {
        message("Invalid 'user.email': ", input)
        message("Quitting")
        return(list())
      }
      git_user_email <- input 
    }
    system(paste0("git config user.email \"", what$user_email, "\""), intern = TRUE)  
  }
  git_user_name <- suppressWarnings(system("git config user.name", intern = TRUE))
  if (!length(git_user_name)) {
    if (!length(what$user_name)) {
      input <- readline("git user.name: ")
      idx <- ifelse(grepl("\\D", input), input, NA)
      if (is.na(idx)) {
        message("Invalid 'user.name': ", input)
        message("Quitting")
        return(list())
      }
      git_user_name <- input 
    }
    system(paste0("git config user.name \"", what$user_name, "\""), intern = TRUE)  
  }

  ## CHANGES //
  tmpfile <- tempfile()
  if (!file.exists("CHANGES.md")) {
    write("", file = "CHANGES.md")
  }
  if (!has_initial_commit) {
#     git_tag <- "\"Initial commit\""
    git_tag <- "\"\""
  } else {
    git_tag <- paste0("\"v", vsn_old)
  }

  tmp_new <- c(
    paste0("# Version ", vsn_new),
    system(paste0("git log --oneline --pretty=format:\" - %s\" ", git_tag, "...HEAD"),
           intern = TRUE),
    "",
    "----------",
    "",
    readLines("CHANGES.md")
  )
  write(tmp_new, file = tmpfile)
  file.rename(from = tmpfile, to = "CHANGES.md")

  ## NEWS //
  tmpfile <- tempfile()
  if (!file.exists("NEWS.md")) {
    write("", file = "NEWS.md")
  }
  news_content <- c(
    if (length(project)) {
      paste0("# CHANGES IN ", project, " VERSION ", vsn_new)
    } else {
      paste0("# CHANGES IN VERSION ", vsn_new)
    },
    "",
    "## NEW FEATURES",
    "",
    "## BUG FIXES",
    "",
    "## MAJOR CHANGES",
    "",
    "## MINOR CHANGES",
    "",
    "## MISC",
    "",
    "-----" ,
    "",
    readLines("NEWS.md")
  )
  write(news_content, file = tmpfile)
  file.rename(from = tmpfile, to = "NEWS.md")
  
  ## HTTPS credentials //
  sys_home <- Sys.getenv("HOME")
  if (sys_home == "") {
    Sys.setenv("HOME" = what$home)
    sys_home <- Sys.getenv("HOME")
  }
  path_netrc <- file.path(sys_home, "_netrc")
  path_netrc_tmp <- gsub("_netrc", "_netrc_0", path_netrc)

  if (!pat) {
    if (!temp_credentials) {
      use_stored_creds <- .ask_useStoredCredentials()
      if (is.null(use_stored_creds)) {
        message("Quitting")
        return(list())
      }
    } else {
      use_stored_cred <- FALSE
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
          "Quitting"
        )
        message(paste(msg, collapse = "\n"))
        return(list())
      }
    } else {
      input <- readline("Username for 'https://github.com': ")
      idx <- ifelse(grepl("\\D", input), input, NA)
      if (is.na(idx)){
        message("Empty username")
        message("Quitting")
        return(list())
      }
      git_https_username <- input
    
      input <- readline(paste0("Password for 'https://", 
        git_user_email, "@github.com': "))
      idx <- ifelse(grepl("\\D", input), input, NA)
      if (is.na(idx)){
        message("Empty password")
        message("Quitting")
        return(list())
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
  } else {
    git_https_username <- devtools::github_pat()
    if (is.null(git_https_username)) {
      msg <- c(
        "No personal access token available",
        "Please provide via environment variable 'GITHUB_PAT'"
      )
      stop(paste(msg, collapse = "\n"))
    }
    git_https_password <- ""
    if (grepl("^https://", git_repos)) {
      git_repos <- gsub("https://", paste0("https://", git_https_username, "@"), git_repos)  
    }
  }

  ## Git commands //
  git_commands <- c(
    "git add --ignore-removal CHANGES.md DESCRIPTION",
    paste0("git commit -m \"Version bump to ", vsn_new, "\""),
    paste0("git tag -a -m \"Tagging version ", vsn_new, "\" \"v", vsn_new, "\""),
    paste0("git push ", git_repos, " --tags")
  )
  
  res <- tryCatch(
    sapply(git_commands, function(cmd) {
      msg <- system(cmd, intern = TRUE)
      message(paste(msg, sep = "\n"))
    }),
    error = function(cond) {
      stop(cond)
    },
    finally = {
      if (file.exists(path_netrc_tmp)) {
        file.rename(from = path_netrc_tmp, to = path_netrc)
      }
      if (temp_credentials) {
        unlink(path_netrc, force = TRUE)
      }
    }
  )
  
  return(list(old = vsn_old, new = vsn_new))
    
  }
)
