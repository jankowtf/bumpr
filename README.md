bumpr
======

Hassle-free version bumping and other things

## Installation

```
require("devtools")
devtools::install_github("Rappster/classr")
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

Bumping from current version `0.1.3` (retrieved from `DESCRIPTION` to a new version.

```
bumpGitVersion()
# Checking remote repository state (this may take a while)
# Current version: 0.1.3
# Suggested version: 0.1.4
# Enter a valid version number [0.1.4=ENTER]: 0.2
# Updating version in DESCRIPTION file to: '0.2?' [yes=ENTER/no]:
# Ready to commit to git? [yes=ENTER/no]:
# Remote git repository (hit ENTER for default: 'origin'):
# Using remote git repository: origin
# Use stored HTTPS credentials (or type them instead) [yes=ENTER/no]:
#
# [master 9955721] Version bump to 0.2 2 files changed, 8 insertions(+), 2 deletions(-)
#
# To https://github.com/Rappster/bumpr * [new tag]         v0.2 -> v0.2
# [1] "0.2"
```

#### Explanation what just happened

- The suggestion always takes the last version digit and increments it by one.
If you simply hit enter, this suggested version will be used.

- However, you are of course free to provide any version you'd like as long as
it complies with the [semtatic versioning conventions](http://semver.org/).

- You are then asked if you want to update your `DESCRIPTION` file (fields `Version` and `Date`).

- Then a last check before commencing with Git/GitHub related stuff is made.

- Based on the specification of your remote repository (**note that this must
have been defined prior to running `bumpGitVersion()`**) a new commit
is issued and **after** that a new tag corresponding to `v<new-version>` (e.g. `v0.2`) is created so **future** commits are automatically tagged with it.

- What also happens is that the file `CHANGES.md` is updated.

- Before pushing to the remote GitHub repository, you are asked how you'd like
to specify your HTTP credentials: either by looking up the information in file
`_netrc` in your `HOME` directory or by typing it into the console.

  This is also where argument `temp_credentials` comes into play: when `TRUE`
  the function makes sure that file `_netrc` is deleted after the version bump is
  finished.

- The function finally returns the new version as a `character` string. If
along the way something when wrong (wrong user input) or when you wanted to exit on purpose, the function returns `character()`.
