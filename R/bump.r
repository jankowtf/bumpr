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
  to <- ns$version    
    
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
    
  ##----------------------------------------------------------------------------
  ## Essential git stuff //
  ##----------------------------------------------------------------------------
  
  res <- suppressWarnings(system("git log", intern = TRUE))
  if (grepl("fatal: Not a git repository", res[1])) {
    is_git_project <- FALSE
  } else {
    is_git_project <- TRUE
  }
  if (!is_git_project) {
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
    input <- readline("Perform initial commit or exit? [commit=ENTER/exit]: ")
    input <- ifelse(grepl("\\D", input), input, "commit")
    if (!grepl("commit|exit", input)){
      message(paste0("Invalid input: ", input))
      message("Exiting")
      return(character())
    }
    if (grepl("exit", input)) {
      message("You should take care of an initial commit.")
      message("Exiting")
      return(character())
    } 
    ## TODO: ensure .gitignore before initial commit
    if (!file.exists(".gitignore")) {
      input <- readline("Add a '.gitignore' file (highly recommended)? [yes=ENTER/no]: ")
      input <- ifelse(grepl("\\D", input), input, "yes")
      if (grepl("[nN]|No|no|NO", input)) {
        add_gitignore <- FALSE
      } else if (!grepl("[yY]|Yes|yes|YES", input)) {
        add_gitignore <- TRUE
      }
      if (add_gitignore) {
        input <- readline("Modify, save and close [hit ENTER]: ")
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
        if (.Platform['OS.type'] == "windows"){
          shell.exec(tmpfile)
        } else {
          system(paste(Sys.getenv("R_BROWSER"), tmpfile))
        }
        file.rename(from = tmpfile, to = ".gitignore")
      }
    }
    res <- system("git add --all", intern = TRUE)
    res <- system("git commit -m \"Initial commit\"", intern = TRUE)
    res <- suppressWarnings(system("git log", intern = TRUE))
    if (grepl("fatal: bad default revision 'HEAD'", res)) {
      message("Something went wrong with initial commit")
      return(character())
    }
  }
  ## Validate repository //
  git_remote <- system("git remote", intern = TRUE)
  if (!length(git_remote)) {
    message("No remote git repositories set yet. Specify at least 'origin'")
    message("Exiting")
    return(character())
  } 
  ## Check remote state //
  message("Checking remote repository state (this may take a while")
  head_check <- sapply(git_remote, function(rem) {
    res <- system(paste0("git ls-remote --heads ", rem), intern = TRUE)
    if (!length(res)) {
      message(paste0("No branches yet in remote repository: ", rem))
      input <- readline(paste0("Would you like to push to ", rem, "? [yes=ENTER/no]: "))
      input <- ifelse(grepl("\\D", input), input, "yes")
      if (grepl("[nN]|No|no|NO", input)) {
        push_to_remote <- FALSE
      } else if (grepl("[yY]|Yes|yes|YES", input)) {
        push_to_remote <- TRUE
      } else {
        message(paste0("Invalid input: ", input))
        message("Exiting")
        return(character())
      }
      if (push_to_remote) {
        system(paste0("git push ", rem, " master"))
        res <- system(paste0("git ls-remote --heads ", rem), intern = TRUE)
      }
    }
    res
  })
  
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
  input <- readline(paste0("Updating version in DESCRIPTION file to: '", 
    vsn_new, "?' [yes=ENTER/no]: "))
  input <- ifelse(grepl("\\D", input), tolower(input), "yes")
  if (grepl("[nN]|No|no|NO", input)) {
    message("Exiting")
    return(character())
  } else if (!grepl("[yY]|Yes|yes|YES", input)) {
    message(paste0("Invalid input: ", input))
    message("Exiting")
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
  
  input <- readline(paste0("Ready to commit to git? [yes=ENTER/no]: "))
  input <- ifelse(grepl("\\D", input), input, "yes")
  if (grepl("[nN]|No|no|NO", input)) {
    message("Exiting")
    return(character())
  } else if (!grepl("[yY]|Yes|yes|YES", input)) {
    message(paste0("Invalid input: ", input))
    message("Exiting")
    return(character())
  }
  
  ## Remote repository //
  input <- readline(paste0("Remote git repository (hit ENTER for default: 'origin'): "))
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
    return(character())
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
        return(character())
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
        return(character())
      }
      git_user_name <- input 
    }
    system(paste0("git config user.name \"", ns$user_name, "\""))  
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

  vsn_0 <- "Initial commit"
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
  
  ## HTTPS credentials //
  sys_home <- Sys.getenv("HOME")
  if (sys_home == "") {
    Sys.setenv("HOME" = ns$home)
    sys_home <- Sys.getenv("HOME")
  }
  path_netrc <- file.path(sys_home, "_netrc")
  path_netrc_tmp <- gsub("_netrc", "_netrc_0", path_netrc)
  
  if (!temp_credentials) {
    msg <- paste0("Use stored HTTPS credentials (or type them instead) [yes=ENTER/no]: ")
    input <- readline(msg)
    input <- ifelse(grepl("\\D", input), input, "yes")
    if (grepl("[nN]|No|no|NO", input)) {
      use_stored_creds <- FALSE
    } else if (grepl("[yY]|Yes|yes|YES", input)) {
      use_stored_creds <- TRUE
    } else {
      message(paste0("Invalid input: ", input))
      message("Exiting")
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
        "Exiting"
      )
      message(paste(msg, collapse = "\n"))
      return(character())
    }
  } else {
    input <- readline("Username for 'https://github.com': ")
    idx <- ifelse(grepl("\\D", input), input, NA)
    if (is.na(idx)){
      message("Empty username")
      message("Exiting")
      return(character())
    }
    git_https_username <- input
  
    input <- readline(paste0("Password for 'https://", 
      git_user_email, "@github.com': "))
    idx <- ifelse(grepl("\\D", input), input, NA)
    if (is.na(idx)){
      message("Empty password")
      message("Exiting")
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
    "git add --ignore-removal CHANGES DESCRIPTION",
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
