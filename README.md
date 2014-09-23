bumpr
======

Hassle-free version bumping and other things

## Installation

```
require("devtools")
devtools::install_github("Rappster/bumpr")
require("bumpr")
```

## Quick intro

### Assumptions

Please note the following assumptions that the current main function, `bumpGitVersion()`, makes:


1. **R package project:**

   You are using this function to systematically manage the versions
of an *R package* project.

2. **Local Git repository:**

   Your package project is under Git version control, i.e. a local Git
repository has been created in your package project's root directory.

  Look for directory `.git` in your package project's root directory.

3. **Remote Git repository:**

   At the very least one remote repository with name `origin` has been defined for your local Git repository.

    Additional remote repositories with different names are not a problem. You can choose them interactively. Run `git remote` in your git shell to find out about your remote repositories

4. **HTTP credentials:**

   If you want to push to a GitHub repository or any other remote repository that relies on HTTPS for authentication, the function assumes that you **are willing to store (at least temporarily) your HTTP credentials in this file**:

  ```
  file.path(Sys.getenv("HOME"), "_netrc")
  ```

  Currently only tested for GitHub repositories as this is the location where the API seems to expect HTTP credentials when pushing to such a repository.

  **However, You can choose to destroy this file after each bump by setting**:

  ```
  temp_credentials = TRUE
  ```

  I will try to find better ways of handling HTTPS credentials in future releases.

### Recommendation

- Initial commit

  Make sure that you already issued an initial commit for your *local*
repository and pushed this to your remote repository.
The function does have built-in checks for very early stages of a
Git repository (i.e. no commits yet, no `.gitignore` file yet,
no branches on the remote repository yet), but I would not consider
this completely stable and tested yet. If you want to check out what
the function does in such early stages, I would recommend testing it
with a toy Git/GitHub repository first

### See version bumping in action

That being said, have fun with easy version bumping and let me know what you 
think.

```
bumpGitVersion()
```
