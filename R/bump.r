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
#'   	\link[bumpr]{bump-GitVersion.S3-character-character-method}
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
#' See generic: \code{\link[bumpr]{bump}}.
#' See main method: \code{\link[bumpr]{bump-RPackageVersion.S3-character-character-method}}.
#'      
#' @inheritParams bump
#' @param what \code{\link{RPackageVersion.S3}}.
#' @param from \code{\link{missing}}.
#' @param to \code{\link{missing}}.
#' @return See method
#'    \code{\link[bumpr]{bump-character-character-RPackageVersion.S3-method}}
#' @example inst/examples/bump.r
#' @seealso \code{
#'    \link[bumpr]{bump},
#'    \link[bumpr]{bump-RPackageVersion.S3-character-character-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bump", 
  signature = signature(
    what = "RPackageVersion.S3",
    from = "missing",
    to = "missing"
  ), 
  definition = function(
    what,
    from,
    to,
    desc_fields = list("Date" = NULL),
    taken = character(),
    sys_state = bumpr::SystemState.S3(),
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
    desc_fields = desc_fields,
    taken = taken, 
    sys_state = sys_state,
    ...
  ))
    
  }
)

#' @title
#' Bump Package Version Number
#'
#' @description 
#' See generic: \code{\link[bumpr]{bump}}
#'      
#' @inheritParams bump
#' @param what \code{\link{RPackageVersion.S3}}.
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
#' @return \code{\link{list}}. (\code{old} and \code{new} version number or 
#'    \code{list()} if the function exited before completing the bump.
#' @example inst/examples/bump.r
#' @seealso \code{
#'    \link[bumpr]{bump}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bump", 
  signature = signature(
    what = "RPackageVersion.S3",
    from = "character",
    to = "character"
  ), 
  definition = function(
    what,
    from,
    to,
    desc_fields = list("Date" = NULL),
    taken = character(),
    sys_state = bumpr::SystemState.S3(),
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
  .rollbackChangesInDescription <- function(sys_state) {
    out <- FALSE
    if (length(sys_state$description_old)) {
      message("Rolling back changes in 'DESCRIPTION'")
      write.dcf(as.data.frame(sys_state$description_old), file = "DESCRIPTION")  
      out <- TRUE
    }
    return(out)
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
  desc <- as.list(read.dcf("DESCRIPTION")[1,])
  
  ## Save old state of DESCRIPTION //
  if (!missing(sys_state)) {
    sys_state$description_old <- desc
  }
  
  out <- tryCatch({
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
      
#       stop("Intentional error for unit testing")
      
      ## Write DESCRIPTION FILE
      write.dcf(as.data.frame(desc), file = "DESCRIPTION")  
    }
    list(old = vsn_old, new = vsn_new)
    },
    error = function(cond) {
      message("Version bump failed")
      .rollbackChangesInDescription(sys_state = sys_state)
      stop(cond)
    }
  )

  return(out)
    
  }
)

#' @title
#' Bump Git Version Number
#'
#' @description 
#' See generic: \code{\link[bumpr]{bump}}.
#' See main method: \code{\link[bumpr]{bump-GitVersion.S3-character-character-method}}.
#'      
#' @inheritParams bump
#' @param what \code{\link{GitVersion.S3}}.
#' @param from \code{\link{missing}}.
#' @param to \code{\link{missing}}.
#' @param temp_credentials \code{\link{logical}}.
#'    \code{TRUE}: delete HTTPS credentials after each bump;
#'    \code{FALSE}: permanently store HTTPS credentials in \code{_netrc} file.
#'    See details.   
#' @return See method
#'    \code{\link[bumpr]{bump-GitVersion.S3-character-character-method}}
#' @example inst/examples/bump.r
#' @seealso \code{
#'    \link[bumpr]{bump},
#'    \link[bumpr]{bump-GitVersion.S3-character-character-method}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bump", 
  signature = signature(
    what = "GitVersion.S3",
    from = "missing",
    to = "missing"
  ), 
  definition = function(
    what,
    from,
    to,
    project = character(),
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
    project = project,
    temp_credentials = temp_credentials,
    ...
  ))
    
  }
)

#' @title
#' Bump Git Version Number
#'
#' @description 
#' See generic: \code{\link[bumpr]{bump}}
#'   	 
#' @inheritParams bump
#' @param what \code{\link{GitVersion.S3}}.
#' @param from \code{\link{character}}.
#' @param to \code{\link{character}}.
#' @return \code{\link{list}}. (\code{old} and \code{new} version number and
#'    the git tag (\code{git_tag}) or 
#'    \code{list()} if the function exited before completing the bump.
#' @example inst/examples/bump.r
#' @seealso \code{
#'    \link[bumpr]{bump}
#' }
#' @template author
#' @template references
#' @export
setMethod(
  f = "bump", 
  signature = signature(
    what = "GitVersion.S3",
    from = "character",
    to = "character"
  ), 
  definition = function(
    what,
    from,
    to,
    project,
    temp_credentials,
    ...
  ) {
    
  ## Private functions //
  .processUserInput <- function(
    input, 
    dflt = "yes",
    sys_state
  ) {
    input <- ifelse(grepl("\\D", input), tolower(input), dflt)
    if (grepl("[qQ]|Quit|quit|QUIT", input)) {
      out <- NULL
      sys_state$quit <- TRUE
    } else if (grepl("[nN]|No|no|NO", input)) {
      out <- FALSE
    } else if (grepl("[yY]|Yes|yes|YES", input)) {
      out <- TRUE
    } else {
      message(paste0("Invalid input: ", input))
      out <- NULL
      sys_state$quit <- TRUE
    }
    return(out)
  }    
  .ask_gitAddGitignoreFile <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Add a '.gitignore' file (highly recommended)? [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes", sys_state = sys_state)
    } else {
      out <- force
    }
    return(out)
  }  
  .ask_gitChangeOrSetRemote <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Change remote name or set? [(c)hange | (s)et | (q)uit]: ")
      input <- ifelse(grepl("\\D", input), input, "change")
      if (grepl("[qQ]|Quit|quit|QUIT", input)) {
        out <- NULL
        sys_state$quit <- TRUE
      } else if (grepl("[cC]|Change|change|CHANGE", input)) {
        out <- "change"
      } else if (grepl("[sS]|set|Set|SET", input)) {
        out <- "set"
      } else {
        message(paste0("Invalid input: ", input))
        out <- NULL
        sys_state$quit <- TRUE
      } 
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitChangePat <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Change current PAT? [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes", sys_state = sys_state)
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitDoInitialCommit <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Perform initial commit (or quit)? [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes", sys_state = sys_state)
    } else {
      out <- force
    }
    return(out)
  }  
  .ask_gitGlobalOrLocalCredentials <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Git 'user.email' and 'user.name': global or local? [(g)lobal | (l)ocal | (q)uit]: ")
      input <- ifelse(grepl("\\D", input), tolower(input), "global")
      if (grepl("[qQ]|quit|Quit|QUIT", input)) {
        out <- NULL
        sys_state$quit <- TRUE
      } else if (grepl("[gG]|global|Global|GLOBAL", input)) {
        out <- "global"
      } else if (grepl("[lL]|local|Local|LOCAL", input)) {
        out <- "local"
      } else {
        message(paste0("Invalid input: ", input))
        out <- NULL
        sys_state$quit <- TRUE
      }
    } else {
      out <- force
    }
    sys_state$global_or_local <- out
    return(out)
  }
  .ask_gitPushToRemote <- function(
    remote = ifelse(!length(sys_state$remote_name), "origin", sys_state$remote_name),
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline(paste0("Would you like to push to ", remote, 
                               "? [(y)es | (n)o | (q)uit]: "))
      out <- .processUserInput(input = input, dflt = "yes", sys_state = sys_state)
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitRemoteName <- function(
    force = character(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Name of remote git repository (hit ENTER for default = 'origin'): ")
      input <- ifelse(grepl("\\D", input), input, "origin")
      if (input == "") {
        message("Invalid input")
        out <- NULL
        sys_state$quit <- TRUE
      } else {
        out <- input
      } 
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitReadyToBumpVersion <- function(
    force = logical(), 
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Ready to bump version in git?' [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes", sys_state = sys_state)
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitSetRemote <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Would you like to add a remote repository now? [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes", sys_state = sys_state)
    } else {
      out <- force
    }
    sys_state$with_remote <- out
    return(out)
  }
  .ask_gitShowPat <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Show current PAT to verify its correctness? [(n)o | (y)es | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "no", sys_state = sys_state)
    } else {
      out <- force
    }
    return(out)
  }  
  .ask_gitUsePat <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Use PAT or 'basic' HTTPS authentication? [(p)at | (b)asic | (q)uit]: ")
      input <- ifelse(grepl("\\D", input), tolower(input), "pat")
      if (grepl("[qQ]|quit|Quit|QUIT", input)) {
        out <- NULL
        sys_state$quit <- TRUE
      } else if (grepl("[pP]|pat|Pat|PAT", input)) {
        out <- "pat"
      } else if (grepl("[bB]|basic|Basic|BASIC", input)) {
        out <- "basic"
      } else {
        message(paste0("Invalid input: ", input))
        out <- NULL
        sys_state$quit <- TRUE
      }
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitValidatedRemoteName <- function(
    remote = character(),
    sys_state
  ) {
    remote_this <- .ask_gitRemoteName(sys_state = sys_state)
    message(paste0("Using remote git repository: ", remote_this))
  
    if (!length(remote) || !remote_this %in% remote) {
      message("No such remote repository: ", remote_this)
      if (length(remote)) {
        message(paste0("Available remote repositories: ", 
          paste(remote, collapse = ", ")))  
      }
      remote_dec <- .ask_gitChangeOrSetRemote(sys_state = sys_state)
      if (sys_state$quit) {
        return(NULL)
      } else if (remote_dec == "change") {
        remote_this <- .ask_gitValidatedRemoteName(
          remote = remote,
          sys_state = sys_state
        )
      } else if (remote_dec == "set") {
        .gitSetRemote(remote = remote_this, sys_state = sys_state)
        push_to_remote <- .ask_gitPushToRemote(
          remote = remote_this,
          sys_state = sys_state
        )
        if (sys_state$quit) {
          return(NULL)
        }
        if (push_to_remote) {
          res <- .gitPushToRemote(remote = remote_this, sys_state = sys_state)
          if (sys_state$quit) {
            return(NULL)
          }
        }
      }
    }
    return(remote_this)
  }
  .ask_useStoredCredentials <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Use stored HTTPS credentials (no = type them)? [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes", sys_state = sys_state) 
    } else {
      out <- force
    }
    return(out)
  }
  .ask_gitWithRemote <- function(
    force = logical(),
    sys_state
  ) {
    if (!length(force)) {
      input <- readline("Would you like to include a remote repository? [(y)es | (n)o | (q)uit]: ")
      out <- .processUserInput(input = input, dflt = "yes", sys_state = sys_state)
    } else {
      out <- force
    }
    sys_state$with_remote <- out
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
  .gitCheckForBranchesInRemote <- function(
    remote = ifelse(!length(sys_state$remote_name), "origin", sys_state$remote_name),
    sys_state
  ) {
    res <- system(paste0("git ls-remote --heads ", remote), intern = TRUE)
    if (!length(res) || grepl("bad default revision", res)) {
      message(paste0("No branches yet in remote repository: ", remote))
      push_to_remote <- .ask_gitPushToRemote(remote = remote, sys_state = sys_state)
      if (sys_state$quit) {
        message("Ensure you pushed at least once to the remote repository")
        return(NULL)
      }
      if (push_to_remote) {
        .gitPushToRemote(remote = remote, sys_state = sys_state)
      }
    }
    res
  }
  .gitDoInitialCommit <- function(
    check_gitignore = FALSE,
    sys_state
  ) {
    if (!file.exists(".gitignore") && check_gitignore) {
      add_gitignore <- .ask_gitAddGitignoreFile(sys_state = sys_state)
      if (sys_state$quit) {
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
      sys_state$quit <- TRUE
      return(NULL)
    }  
    res
  }
#   input <- "To https://e6e3de550c3358f20bd8533c52e9ab20821d20bf@github.com/Rappster/bumpr.test
#  - [deleted]         v0.1.0"
  .gitEnsurePatIsHidden <- function(
    input = character(),
    pattern = paste0(Sys.getenv("GITHUB_PAT"), "@")
  ) {
    if (length(input) && any(grepl(pattern, input))) {
      input <- gsub(pattern, "", input)
    }
    return(input)
  }
  .gitExistsRemoteRepository <- function(
    remote = ifelse(!length(sys_state$remote_name), "origin", sys_state$remote_name),
    sys_state
  ) {
    res <- suppressWarnings(system(paste0("git ls-remote ", remote),
                                   intern = TRUE))
    if (grepl("remote: Repository not found", res[1])) {
      out <- FALSE
    } else {
      out <- TRUE
    }
    return(out)
  }
  .gitGetNetrcFileNames <- function(sys_state) {
    sys_home <- Sys.getenv("HOME")
    if (sys_home == "") {
      Sys.setenv("HOME" = what$home)
      sys_home <- Sys.getenv("HOME")
    }
    path_netrc <- file.path(sys_home, "_netrc")
    path_netrc_tmp <- gsub("_netrc", "_netrc_0", path_netrc)
  
    sys_state$path_netrc <- path_netrc
    sys_state$path_netrc_tmp <- path_netrc_tmp
  }
  .gitGetRemoteAll <- function(sys_state) {
    git_remote <- system("git remote -v", intern = TRUE)
    git_remote_url <- gsub(".*\\t", "", git_remote, perl = TRUE)
    git_remote_url <- unique(gsub(" \\((fetch|push)\\)", "", 
      git_remote_url, perl = TRUE))
    git_remote_name <- unique(gsub("\\t.*", "", git_remote, perl = TRUE))
    git_remote <- as.list(git_remote_url)
    names(git_remote) <- git_remote_name
    sys_state$remote_all <- git_remote
    return(git_remote)
  }
  .gitGetRemoteThis <- function(sys_state) {
    remote_this <- .ask_gitValidatedRemoteName(remote = names(sys_state$remote_all))
    if (sys_state$quit) {
      return(NULL)
    }
    sys_state$remote_url <- sys_state$remote_all[[remote_this]]
    sys_state$remote_name <- remote_this
    sys_state$remote <- remote_this
    return(remote_this)
  }
  .gitGetUserCredentialsCommand <- function(sys_state) {
    ## Git user credentials //
    if (!length(sys_state$global_or_local)) {
      global_or_local <- .ask_gitGlobalOrLocalCredentials(sys_state = sys_state)
    } else {
      global_or_local <- sys_state$global_or_local
    }
    if (sys_state$quit) {
      return(NULL)
    }
    if (global_or_local == "global") {
      cmd_user_email <- "git config --global user.email"
      cmd_user_name <- "git config --global user.name"
    } else if (global_or_local == "local") {
      cmd_user_email <- "git config user.email"
      cmd_user_name <- "git config user.name"
    }
    sys_state$cmd_user_email <- cmd_user_email
    sys_state$cmd_user_name <- cmd_user_name
    return(sys_state)
  }
  .gitHandleAuthentication <- function(
    what = sys_state$what,
    temp_credentials = sys_state$temp_credentials,
    sys_state
  ) {
    ## Decision if PAT or basic authentication //
    if (!length(sys_state$pat_or_basic)) {
      pat_or_basic <- .ask_gitUsePat(sys_state = sys_state)
      sys_state$pat_or_basic <- pat_or_basic
    } else {
      pat_or_basic <- sys_state$pat_or_basic
    }
    if (sys_state$quit) {
      return(NULL)
    }
    if (pat_or_basic == "basic") {
    ## Basic authentication //      
      .gitGetNetrcFileNames(sys_state = sys_state)

      if (!temp_credentials) {
        use_stored_creds <- .ask_useStoredCredentials(sys_state = sys_state)
        if (sys_state$quit) {
          return(NULL)
        }
      } else {
        use_stored_creds <- FALSE
      }
      
      if (use_stored_creds) {
      ## Use stored credentials //
        if (!file.exists(path_netrc)) {
          msg <- c(
            "Missing file '_netrc' with HTTPS credentials",
            paste0("Create that file in: ", sys_home),
            "Make sure it contains the following content:",
            "  machine github.com",
            "  login <your-username>",
            "  password <your-password>",
            "  protocol https"
          )
          message(paste(msg, collapse = "\n"))
          sys_state$quit <- TRUE
          return(NULL)
        }
      } else {
      ## Type credentials //
        input <- readline("Username for 'https://github.com': ")
        idx <- ifelse(grepl("\\D", input), input, NA)
        if (is.na(idx)){
          message("Empty username")
          sys_state$quit <- TRUE
          return(NULL)     
        }
        git_https_username <- input
      
        input <- readline(paste0("Password for 'https://", 
          git_user_email, "@github.com': "))
        idx <- ifelse(grepl("\\D", input), input, NA)
        if (is.na(idx)){
          message("Empty password")
          sys_state$quit <- TRUE
          return(NULL)
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
    } else if (pat_or_basic == "pat") {
    ## Authentication via PAT //      
      git_pat <- Sys.getenv("GITHUB_PAT")
      if (git_pat == "") {
      ## No PAT available yet //
        input <- readline("Provide personal authentication token (PAT): ")
        idx <- ifelse(grepl("\\D", input), input, NA)
        if (is.na(idx)){
          message("Invalid input")
          sys_state$quit <- TRUE
          return(NULL)
        }
        git_pat <- input
        Sys.setenv("GITHUB_PAT" = git_pat)
      } else {
      ## PAT available //
        check_pat <- .ask_gitShowPat(sys_state = sys_state)
        if (sys_state$quit) {
          return(NULL)
        }
        change_pat <- FALSE
        if (check_pat) {
          message(sprintf("Current PAT: %s", git_pat))
          change_pat <- .ask_gitChangePat(sys_state = sys_state)
          if (sys_state$quit) {
            return(NULL)
          }
        }
        if (change_pat) {
          input <- readline("Provide personal authentication token (PAT): ")
          idx <- ifelse(grepl("\\D", input), input, NA)
          if (is.na(idx)){
            message("Invalid input")
            sys_state$quit <- TRUE
            return(NULL)
          }
          Sys.setenv("GITHUB_PAT" = git_pat)
        }
      }
  #     git_https_password <- ""
      remote <- sys_state$remote_url
      if (grepl("^https://", remote)) {
        remote <- gsub("https://", paste0("https://", git_pat, "@"), remote)  
        sys_state$remote <- remote
      }
    }
    sys_state$ask_authentication <- FALSE
    return(sys_state)
  }
  .gitIsRemoteRepository <- function(
    remote = ifelse(!length(sys_state$remote_name), "origin", sys_state$remote_name),
    sys_state
  ) {
    res <- suppressWarnings(system(paste0("git ls-remote ", remote),
                                   intern = TRUE))
    patterns <- c(
      "fatal:.*does not appear to be a git repository"
#       "remote: Repository not found"
    ) 
    if (any(sapply(patterns, grepl, res[1]))) {
      out <- FALSE
    } else {
      out <- TRUE
    }
    return(out)
  }
#   .gitIsRemoteRepository()
  .gitIsLocalRepository <- function(sys_state) {
    res <- suppressWarnings(system("git log --oneline", intern = TRUE))
    if (grepl("fatal:.*Not a git repository", res[1])) {
      out <- FALSE
    } else {
      out <- TRUE
    }
    return(out)
  }
  .gitPushToRemote <- function(
    remote = ifelse(!length(sys_state$remote), "origin", sys_state$remote),
    branch = ifelse(!length(sys_state$branch), "master", sys_state$branch), 
    sys_state
  ) {
    if (!.gitIsRemoteRepository(remote = remote, sys_state = sys_state)) {
      message(paste0("Not a git remote repository: ", remote))
      return(NULL)
    }
    
    ## Authentication //
    if (sys_state$ask_authentication) {
      .gitHandleAuthentication(sys_state = sys_state)
      remote <- sys_state$remote
    }
    if (sys_state$quit) {
      message("Authentication aborted")
      return(NULL)
    }
    system(paste0("git push ", remote, " ", branch), intern = TRUE)
    res <- system(paste0("git ls-remote --heads ", remote), intern = TRUE)
  }
  .gitRollbackBumpCommit <- function(sys_state) {
    message("Rolling back bump commit")
    cmd_1 <- paste0("git reset HEAD~1")
    msg <- suppressWarnings(system(cmd_1, intern = TRUE))
    return(msg)
  }
  .gitRollbackTag <- function(sys_state) {
    message("Rolling back Git tags")
    cmd_1 <- paste0("git tag -d ", sys_state$git_tag)
    msg <- suppressWarnings(system(cmd_1, intern = TRUE))
    if (sys_state$with_remote) {
      cmd_2 <- paste0("git push ", sys_state$remote,  " :refs/tags/", 
        sys_state$git_tag)
      msg <- c(msg, suppressWarnings(system(cmd_2, intern = TRUE)))
    }
    
    if (length(idx <- grep("Deleting a non-existent ref", msg))) {
      msg <- msg[-idx]
    }
    if (length(idx <- grep("error:", msg))) {
      msg <- msg[-idx]
    }
    msg <- .gitEnsurePatIsHidden(input = msg)
    
    return(paste(msg, collapse = "\n"))
  }
  .gitSetRemote <- function(
    remote = ifelse(!length(sys_state$remote_name), "origin", sys_state$remote_name),
    url = character(),
    sys_state
  ) {
    if (!length(remote)) {
      input <- readline("Repository name [ENTER='origin']: ")
      remote <- ifelse(grepl("\\D", input), input, "origin")
    } 
    if (!length(url)) {
      input <- readline("Repository URL: ")
      input <- ifelse(grepl("\\D", input), input, NA)
      if (is.na(input)) {
        message("Invalid input")
        sys_state$quit
        return(sys_state)
      }
      url <- paste0("\"", input, "\"")
    }
    msg <- system(paste0("git remote add ", remote, " ", url), intern = TRUE)
    message(msg)
    return(TRUE)
  }
  .gitUserEmail <- function(
    cmd = sys_state$cmd_user_email,
    global_or_local = sys_state$global_or_local,
    sys_state
  ) {
    if (!length(cmd)) {
      .gitGetUserCredentialsCommand(sys_state = sys_state)
    }
    if (!length(global_or_local)) {
      .ask_gitGlobalOrLocalCredentials(sys_state = sys_state)
    }
    git_user_email <- suppressWarnings(system(cmd, intern = TRUE))
    if (!length(git_user_email)) {
      if (!length(sys_state$what$user_email)) {
        if (global_or_local == "global") {
          input <- readline("Provide '--global user.email': ")
        } else if (global_or_local == "local") {
          input <- readline("Provide 'user.email': ")
        }
        idx <- ifelse(grepl("\\D", input), input, NA)
        if (is.na(idx)) {
          message("Invalid input: ", input)
          sys_state$quit <- TRUE
          return(sys_state)
        }
        git_user_email <- input 
      } else {
        git_user_email <- sys_state$what$user_email 
      }
      if (global_or_local == "global") {
        cmd <- paste0("git config --global user.email \"", 
          git_user_email, "\"")
      } else if (global_or_local == "local") {
        cmd <- paste0("git config user.email \"", 
          git_user_email, "\"")
      }
      system(cmd, intern = TRUE)  
    }
    sys_state$git_user_email <- git_user_email
    return(git_user_email)
  }
  .gitUserName <- function(
    cmd = sys_state$cmd_user_name,
    global_or_local = sys_state$global_or_local,
    sys_state
  ) {
    if (!length(cmd)) {
      .gitGetUserCredentialsCommand(sys_state = sys_state)
    }
    if (!length(global_or_local)) {
      .ask_gitGlobalOrLocalCredentials(sys_state = sys_state)
    }
    git_user_name <- suppressWarnings(system(cmd, intern = TRUE))
    if (!length(git_user_name)) {
      if (!length(sys_state$what$user_name)) {
        if (global_or_local == "global") {
          input <- readline("Provide '--global user.name': ")
        } else if (global_or_local == "local") {
          input <- readline("Provide 'user.name': ")
        }
        idx <- ifelse(grepl("\\D", input), input, NA)
        if (is.na(idx)) {
          message("Invalid input: ", input)
          sys_state$quit <- TRUE
          return(sys_state)
        }
        git_user_name <- input 
      } else {
        git_user_name <- sys_state$what$user_name
      }
      if (global_or_local == "global") {
        cmd <- paste0("git config --global user.name \"", 
          git_user_name, "\"")
      } else if (global_or_local == "local") {
        cmd <- paste0("git config user.name \"", 
          git_user_name, "\"")
      }
      system(cmd, intern = TRUE)  
    }
    sys_state$git_user_name <- git_user_name
    return(git_user_name)
  }
  .rollbackChangesInDescription <- function(sys_state) {
    out <- FALSE
    if (length(sys_state$description_old)) {
      message("Rolling back changes in 'DESCRIPTION'")
      write.dcf(as.data.frame(sys_state$description_old), file = "DESCRIPTION")  
      out <- TRUE
    }
    return(out)
  }

  ##----------------------------------------------------------------------------
  ## Initialize sys_state environment //
  ##----------------------------------------------------------------------------

  sys_state <- bumpr::SystemState.S3()
  sys_state$what <- what
  
  ##----------------------------------------------------------------------------
  ## Essential git stuff //
  ##----------------------------------------------------------------------------
  
  if (!.gitIsLocalRepository(sys_state = sys_state)) {
    res <- system("git init", intern = TRUE)
  }
  res <- suppressWarnings(system("git log", intern = TRUE))
  if (grepl("fatal: bad default revision 'HEAD'", res[1])) {
    has_initial_commit <- FALSE
  } else {
    has_initial_commit <- TRUE
  }

  ## Initial commit //
  if (!has_initial_commit) {
    message("No initial commit yet")
    do_initial_commit <- .ask_gitDoInitialCommit(sys_state = sys_state)
    if (sys_state$quit || !do_initial_commit) {
      message("You should take care of an initial commit")
      return(list())
    }
    
    ## Gitignore file //
    if (!file.exists(".gitignore")) {
      add_gitignore <- .ask_gitAddGitignoreFile(sys_state = sys_state)
      if (sys_state$quit) {
        message("Quiting")
        return(list())
      }
      if (add_gitignore) {
        .gitAddGitignoreFile()
      }
    }
    
    ## Actual commit //
    res <- .gitDoInitialCommit(sys_state = sys_state)
    if (sys_state$quit) {
      message("Quiting")
      return(list())
    }
  }

  ## With remote //
  .ask_gitWithRemote(sys_state = sys_state)
  if (sys_state$with_remote) {
    ## Validate remote repository //
    git_remote <- system("git remote", intern = TRUE)
    if (!length(git_remote)) {
      message("No remote git repositories set yet")
      message("Asking")
      .ask_gitSetRemote(sys_state = sys_state)
      if (sys_state$quit) {
  #       message("Make sure you set a remote repository in your local repository")
        message("Quiting")
        return(list())
      }
      if (sys_state$with_remote) {
        .gitSetRemote(remote = character(), sys_state = sys_state)
      }
    }
  }

  ##----------------------------------------------------------------------------
  ## DESCRIPTION file and versions //
  ##----------------------------------------------------------------------------
  
  git_tags <- system("git tag", intern = TRUE)
  res <- bumpPackageVersion(taken = git_tags, sys_state = sys_state)
  if (!length(res)) {
    message("Quitting")
    return(list())
  }
  vsn_new <- res$new
  vsn_old <- res$old
  sys_state$git_tag <- paste0("v", vsn_new)

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
  
  .ask_gitReadyToBumpVersion(sys_state = sys_state)
  if (sys_state$quit) {
    .rollbackChangesInDescription(sys_state = sys_state)
    message("Quitting")
    return(list())
  }
  
  ## Git user credentials //
  .gitGetUserCredentialsCommand(sys_state = sys_state)
  if (sys_state$quit) {
    .rollbackChangesInDescription(sys_state = sys_state)
    message("Quitting")
    return(list())
  }
  .gitUserEmail(sys_state = sys_state)
  .gitUserName(sys_state = sys_state)

  if (sys_state$with_remote) {
    ## Validated remote repository //
    .gitGetRemoteAll(sys_state = sys_state)
    .gitGetRemoteThis(sys_state = sys_state)

    ## Check existence //
    if (!.gitExistsRemoteRepository(
          remote = sys_state$remote_name, 
          sys_state = sys_state)
    ) {
      msg <- c(
        "Remote does not exist yet //",
        paste0("Name: ", sys_state$remote_name),
        paste0("URL: ", sys_state$remote_url),
        "Please ensure existence first"
      )  
      message(paste(msg, collapse = "\n"))
      
      .rollbackChangesInDescription(sys_state = sys_state)
      message("Quitting")
      return(list())
    }

    ## Check validity of remote //
    if (!.gitIsRemoteRepository(remote = sys_state$remote_name, sys_state = sys_state)) {
      .gitCheckForBranchesInRemote(
        remote = sys_state$remote_name,
        sys_state = sys_state
      ) 
      if (sys_state$quit) {
        message(paste0("Not a git remote repository: ", sys_state$remote_name))
        message("Make sure you have initialized either a bare repository or cloned an existing one")
        .rollbackChangesInDescription(sys_state = sys_state)
        message("Quitting")
        return(list())
      }    
    }

    .gitCheckForBranchesInRemote(
      remote = sys_state$remote_name,
      sys_state = sys_state
    ) 
    if (sys_state$quit) {
      message(paste0("No branches in remote: ", sys_state$remote_name))
      .rollbackChangesInDescription(sys_state = sys_state)
      message("Quitting")
      return(list())
    }

    ## Authentication //
    if (sys_state$ask_authentication) {
      .gitHandleAuthentication(
        what = what, 
        temp_credentials = temp_credentials,
        sys_state = sys_state
      )
      if (sys_state$quit) {
        .rollbackChangesInDescription(sys_state = sys_state)
        message("Quitting")
        return(list())  
      }
    }
  }

  ##----------------------------------------------------------------------------
  ## CHANGES //
  ##----------------------------------------------------------------------------
  
  tmpfile_changes <- tempfile()
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
    system(paste0("git log --oneline --pretty=format:\" - %s\" ", 
      git_tag, "...HEAD"), intern = TRUE),
    "",
    "----------",
    "",
    readLines("CHANGES.md")
  )
  write(tmp_new, file = tmpfile_changes)
  
  file.rename(from = "CHANGES.md", to = "CHANGES_0.md")
  file.rename(from = tmpfile_changes, to = "CHANGES.md")
  
  ##----------------------------------------------------------------------------
  ## NEWS //
  ##----------------------------------------------------------------------------

  tmpfile_news <- tempfile()
  if (!file.exists("NEWS.md")) {
    write("", file = "NEWS.md")
  }
  cnt_old <- readLines("NEWS.md")

  update_news <- FALSE
  if (!grepl(paste0("VERSION ", vsn_new), cnt_old[1])) {
    update_news <- TRUE
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
      cnt_old
    )
    write(news_content, file = tmpfile_news)
    file.rename(from = "NEWS.md", to = "NEWS_0.md")
    file.rename(from = tmpfile_news, to = "NEWS.md")
  }

  ## Git commands //
  git_commands <- c(
    "git add --ignore-removal CHANGES.md DESCRIPTION",
    paste0("git commit -m \"Version bump to ", vsn_new, "\""),
    paste0("git tag -a -m \"Tagging version ", vsn_new, "\" \"v", vsn_new, "\""),
    if (sys_state$with_remote) {
      paste0("git push ", sys_state$remote, " --tags")
    } else {
      NULL
    }
  )

  ## Execut git commands //
  res <- tryCatch({
      sapply(seq(along=git_commands), function(cmd) {
        res <- tryCatch({
#             if (cmd == 3) {
#               msg <- c(
#                 "Intentional error for unit testing",
#                 paste0("Git command: ", .gitEnsurePatIsHidden(git_commands[cmd]))
#               )
#               stop(paste(msg, collapse = "\n"))
#             }
            cmd <- git_commands[cmd]
            msg <- system(cmd, intern = TRUE)
            
            ## Make sure PAT is not displayed //
            msg <- .gitEnsurePatIsHidden(input = msg)
            
            message(paste(msg, sep = "\n"))
          },
          error = function(cond) {
            message("Version bump failed")
            .rollbackChangesInDescription(sys_state = sys_state)
            message("Rolling back changes in 'CHANGES.md'")
            file.rename(from = "CHANGES_0.md", to = "CHANGES.md")
            message("Rolling back changes in 'NEWS.md'")
            file.rename(from = "NEWS_0.md", to = "NEWS.md")
            message(.gitRollbackBumpCommit(sys_state = sys_state))
            message(.gitRollbackTag(sys_state = sys_state))
            stop(cond)
          }
        )
      })
      
      ## Cleanup //
      file.remove("CHANGES_0.md")
      file.remove("NEWS_0.md")
    },
    finally = {
      if (sys_state$with_remote) {
        ## Cleanup with respect to '_netrc' file // 
        if (sys_state$pat_or_basic == "basic") {
          if (file.exists(sys_state$path_netrc_tmp)) {
            file.rename(from = sys_state$path_netrc_tmp, to = sys_state$path_netrc)
          }
          if (temp_credentials) {
            unlink(sys_state$path_netrc, force = TRUE)
          }
        }
      }
    }
  )
  
  return(list(old = vsn_old, new = vsn_new, git_tag = sys_state$git_tag))
    
  }
)
