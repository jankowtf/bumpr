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
#'   	\link[reactr]{bump-Bumpr.Git.S3-character-character-method}
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
#' Bump Version
#'
#' @description 
#' See generic: \code{\link[reactr]{bump}}
#'      
#' @inheritParams bump
#' @param what \code{\link{Bumpr.Git.S3}}.
#' @param from \code{\link{missing}}.
#' @param to \code{\link{missing}}.
#' @return See method
#'    \code{\link[reactr]{bump-character-character-Bumpr.Git.S3-method}}
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
    what = "Bumpr.Git.S3",
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
#' Get Value from Environment
#'
#' @description 
#' See generic: \code{\link[reactr]{bump}}
#'   	 
#' @inheritParams bump
#' @param what \code{\link{Bumpr.Git.S3}}.
#' @param from \code{\link{character}}.
#' @param to \code{\link{character}}.
#' @return \code{\link{ANY}}. Variable value
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
    what = "Bumpr.Git.S3",
    from = "character",
    to = "character"
  ), 
  definition = function(
    what,
    from,
    to,
    temp_credentials,
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
  .ask_updateDescriptionFile <- function(force = logical(), vsn) {
    if (!length(force)) {
      input <- readline(paste0("Updating version in DESCRIPTION file to: '", 
        vsn, "?' [(y)es | (n)o | (q)uit]: "))
      out <- .processUserInput(input = input, dflt = "yes")
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
        return(character())
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
#   .gitSetRemote()
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
      return(character())
    }
    
    ## Gitignore file //
    if (!file.exists(".gitignore")) {
      add_gitignore <- .ask_gitAddGitignoreFile()
      if (is.null(add_gitignore)) {
        message("Quiting")
        return(character())
      }
      if (add_gitignore) {
        .gitAddGitignoreFile()
      }
    }
    
    ## Actual commit //
    res <- .gitDoInitialCommit()
    if (is.null(res)) {
      message("Quiting")
      return(character())
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
      return(character())
    }
    .gitSetRemote()
  } 

  ##----------------------------------------------------------------------------
  ## DESCRIPTION file and versions //
  ##----------------------------------------------------------------------------
  
  ## Read description file //
  tmp <- read.dcf("DESCRIPTION")
  desc <- as.list(tmp)
  names(desc) <- colnames(tmp)  
  pkg_name <- desc$Package
  
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
  git_tags <- system("git tag", intern = TRUE)
  .getVersionInput <- function(git_tags) {
    input <- readline(paste0("Enter a valid version number [", vsn_sug, "=ENTER]: "))
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
    ## Check against git tags //
    while (paste0("v", vsn_new) %in% git_tags) {
      message(paste0("Tag for version '", vsn_new, "' already exists"))
      message("Git tags:")
      message(paste(git_tags, collapse="\n"))
      message("Choose another version")
      vsn_new <- .getVersionInput(git_tags = git_tags)
    }
    vsn_new
  }
  vsn_new <- .getVersionInput(git_tags = git_tags)
  
  ## Verify update of DESCRIPTION file //  
  res <- .ask_updateDescriptionFile(vsn = vsn_new)
  if (is.null(res)) {
    message("Quitting")
    return(character())
  }

  ## Update DESCRIPTION FILE
  desc$Version <- vsn_new
  desc$Date <- Sys.time()
  
  ## Write DESCRIPTION FILE
  write.dcf(as.data.frame(desc), file = "DESCRIPTION")
  
  ##----------------------------------------------------------------------------
  ## Git //
  ##----------------------------------------------------------------------------
  
  res <- .ask_gitReadyToBumpVersion()
  if (is.null(res)) {
    message("Quitting")
    return(character())
  }
  
  ## Validated remote repository //
  git_remote <- system("git remote", intern = TRUE)
  git_repos <- .ask_gitValidatedRemoteName(git_remote = git_remote)
  if (is.null(git_repos)) {
    message("Quitting")
    return(character())
  }

  if (!.gitIsRemoteRepository(name = git_repos)) {
    if(is.null(.gitCheckForBranchesInRemote(name = git_repos))) {
      message("Quitting")
      return(character())
    }
    message(paste0("Not a git remote repository: ", git_repos))
    message("Make sure you have initialized either a bare repository or cloned an existing one")
    message("Quitting")
    return(character())
  }
  if(is.null(.gitCheckForBranchesInRemote(name = git_repos))) {
    message("Quitting")
    return(character())
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
        return(character())
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
        return(character())
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
    git_tag <- paste0("\"v", vsn_0)
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
    paste0("# CHANGES IN ", pkg_name, " VERSION ", vsn_new),
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
    "-----"
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
  
  if (!temp_credentials) {
    use_stored_creds <- .ask_useStoredCredentials()
    if (is.null(use_stored_creds)) {
      message("Quitting")
      return(character())
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
      return(character())
    }
  } else {
    input <- readline("Username for 'https://github.com': ")
    idx <- ifelse(grepl("\\D", input), input, NA)
    if (is.na(idx)){
      message("Empty username")
      message("Quitting")
      return(character())
    }
    git_https_username <- input
  
    input <- readline(paste0("Password for 'https://", 
      git_user_email, "@github.com': "))
    idx <- ifelse(grepl("\\D", input), input, NA)
    if (is.na(idx)){
      message("Empty password")
      message("Quitting")
      return(character())
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
  
  return(vsn_new)
    
  }
)
