bumpr
======

Easy systematic version bumping and more

## Installation

```
require("devtools")
devtools::install_github("Rappster/classr")
devtools::install_github("Rappster/bumpr")
require("bumpr")
```

## Quick intro

See `?bumpr` for the overall purpose of this package.

## Bump R package version (no Git required)

Bumping from current version `0.3.2` (retrieved from `DESCRIPTION`) to a new version

```
bumpGitVersion()
# Current version: 0.3.1
# Suggested version: 0.3.2
# Enter a valid version number: [ENTER = 0.3.2] 
# Using suggested version: 0.3.2
# Updating version in DESCRIPTION file to '0.3.2?' [(y)es | (n)o | (q)uit]: 
# $old
# [1] "0.3.1"
# 
# $new
# [1] "0.3.2"
```

#### Explanation what just happened

- The suggestion always takes the last version digit and increments it by one.
If you simply hit enter, this suggested version will be used.

- However, you are of course free to provide any version you'd like as long as
it complies with the [semtatic versioning conventions](http://semver.org/).

- You are then asked if you want to update your `DESCRIPTION` file (fields `Version` and `Date`).

- The function returns the old and new version as a `list` string. If
along the way something when wrong (wrong user input) or when you wanted to quit on purpose, the function returns `list()`.

## Bump Git version 

**Please read the assumptions and recommendations stated in `?bumpGitVersion()`
before you are running this function!**

As `bumpPackageVersion()` is called inside `bumpGitVersion()` and we already
ran `bumpPackageVersion()` explicitly, we'll stay at the same (new) version
number when prompted. If you just run `bumpGitVersion()` make your choices
accordingly.

Recommended way of using this function when version-controlling your package
project with Git: run `bumpGitVersion()` **without** previously running
`bumpPackageVersion()`

```
bumpGitVersion()
# Taken versions (last 10): 
# 0.1.0.12
# 0.1.0.13
# 0.1.0.14
# 0.1.1
# 0.1.3
# 0.2
# 0.2.1
# 0.2.2
# 0.2.3
# 0.2.4
# 0.3
# 0.3.1
# Current version: 0.3.2
# Suggested version: 0.3.3
# Enter a valid version number: [ENTER = 0.3.3] 0.3.2
# Ready to bump version in git?' [(y)es | (n)o | (q)uit]: 
# Name of remote git repository (hit ENTER for default = 'origin'): 
# Using remote git repository: origin
# Use stored HTTPS credentials (no = type them)? [(y)es | (n)o | (q)uit ]: 
# 
# [feature-refactorBumpGitVersion 687df71] Version bump to 0.3.2 2 files changed, 13 insertions(+), 5 deletions(-)
# 
# To https://github.com/Rappster/bumpr * [new tag]         v0.3.2 -> v0.3.2
# $old
# [1] "0.3.2"
# 
# $new
# [1] "0.3.2"
```

#### Explanation what just happened

- Consider:
  - If you have **not** run `bumpPackageVersion()` prior to calling this 
    function (the recommended way if you are version-controlling your package
    project with Git), the simply follow the instructions.
  - If you **have** run `bumpPackageVersion()` explicitly prior to calling this
    function, then re-enter the **OLD** version (unless you want to bump the package version number yet again).

- Then a last check before commencing with Git/GitHub related stuff is made.

- Based on the specification of your remote repository (**note that this should
have been defined prior to running `bumpGitVersion()`**) a new commit
is issued and **after** that a new tag corresponding to `v<new-version>` (e.g. `v0.3.2`) is created so **future** commits are automatically tagged with it.

- What also happens is that the files `CHANGES.md` and \code{NEWS.md} are updated
  as described in `?bumpr`.

- Before pushing to the remote (GitHub) repository, you are asked how you'd like
to specify your HTTP credentials: either by looking up the information in file
`_netrc` in your `HOME` directory or by typing it into the console. 

  This is also where argument `temp_credentials` comes into play: when `TRUE`
  the function makes sure that file `_netrc` is deleted after the version bump is
  finished.

  If your remote repository does not require HTTPS authentication, simply hit `ENTER`.

- The function returns the old and new version as a `list` string. If
along the way something when wrong (wrong user input) or when you wanted to quit on purpose, the function returns `list()`.
