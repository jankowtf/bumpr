bumpr (v0.3.12)
======

Easy systematic version bumping and more

## Installation

```
require("devtools")
devtools::install_github("Rappster/bumpr")
require("bumpr")
```

## Overview

See `?bumpr` for the overall purpose of this package.

## Bump R package version (no Git required)

Retrieves the current package version from the `DESCRIPTION` file,
suggests the next version number and prompts the user for a new 
version number. After asking permission, the new version number is 
written to the `DESCRIPTION` file along with the additional
information provided via `desc_fields`. Currently, only 
an element of form `Date = NULL` is allowed/used, which 
corresponds to also updating the `Date` field of the 
`DESCRIPTION` file to the current system time. `desc_fields = list()` suppresses that.

### Example

Bumping from current version `0.3.3` to a new version

```
bumpPackageVersion()
# Taken versions numbers (last 10): 
# 0.3.1
# 0.3.2
# 0.3.3
# 0.3.4
# 0.3.5
# 0.3.6
# 0.3.7
# 0.3.8
# 0.3.9
# 0.3.10
# 0.3.11
# Current version: 0.3.11
# Suggested version: 0.3.12
# Enter a valid version number: [ENTER = 0.3.12] 
# Using suggested version: 0.3.12
# Updating version in DESCRIPTION file to '0.3.12?' [(y)es | (n)o | (q)uit]: 
# $old
# [1] "0.3.11"
# 
# $new
# [1] "0.3.12"
```

#### Explanation what just happened

- The suggestion always takes the last version digit and increments it by one.
If you simply hit enter, this suggested version will be used. 

  However, you are of course free to provide any version you'd like as long as
it complies with the [semtatic versioning conventions](http://semver.org/).

- You are then asked if you want to update your `DESCRIPTION` file (currently updated fields: `Version` and `Date`).

- The function returns the old and new version as a `list`. If
along the way something when wrong (wrong user input) or when you wanted to quit on purpose, the function returns `list()`.

## Bump Git version 

Performs all sorts of Git-related checks and tasks in order to take care
that everything necessary is done that is related to bumping a project
to a higher version number.
 
This provided version number is transferred to `v{version-number}`,
e.g. `v0.1.1`, and added as a Git tag. 

All commits linked to the *previous* version/tag are queried and added
to file `CHANGES.md`. Additionally, a template section to state the 
changes in the *new* version is added in file `NEWS.md`.

Files `DESCRIPTION` and `CHANGES.md` are automatically 
commited to signal the version bump.

Optionally, you can push the new version (i.e. the new tag) as well 
as the associated commit to a remote repository (default: `origin`).
This can be any valid Git remote repository, including a GitHub
repository

**Please read the assumptions and recommendations stated in `?bumpGitVersion()`
before you are running this function!**

Differentiate use cases:

1. Calling `bumpPackageVersion()` prior to calling `bumpGitVersion()`

  In this case, you most likely *do not*  want to bump to yet another version, so you should **stay** at the version you bumped to via `bumpPackageVersion()`, i.e. re-type the old version number instead of using the suggested incremented version number.

  `bumpPackageVersion()` is called inside of `bumpGitVersion()`, so bumping to a new version number again would be misleading. I hope to be able to implement an automatic detection for this in a future release.

2. Calling `bumpGitVersion()` without prior calling of `bumpPackageVersion()`

  This is the recommended way of using this function when version-controlling your package project with Git. Make your version specification as you see fit.

### Example

The example assumes that `bumpGitVersion()` is called without previously having called `bumpPackageVersion()` explicitly.

```
bumpGitVersion()

# Taken versions numbers (last 10): 
# 0.3.1
# 0.3.2
# 0.3.3
# 0.3.4
# 0.3.5
# 0.3.6
# 0.3.7
# 0.3.8
# 0.3.9
# 0.3.10
# 0.3.11
# Current version: 0.3.11
# Suggested version: 0.3.12
# Enter a valid version number: [ENTER = 0.3.12] 
# Using suggested version: 0.3.12
# Updating version in DESCRIPTION file to '0.3.11?' [(y)es | (n)o | (q)uit]: 
# Ready to bump version in git?' [(y)es | (n)o | (q)uit]: 
# Name of remote git repository (hit ENTER for default = 'origin'): 
# Using remote git repository: origin
# Git 'user.email' and 'user.name': global or local? [(g)lobal | (l)ocal | (q)uit ]: 
# Use PAT or 'basic' HTTPS authentication? [(p)at | (b)asic | (q)uit ]: 
# Show current PAT to verify its correctness? [(n)o | (y)es | (q)uit]: y
# Current PAT: {pat-value}
# Change current PAT? [(y)es | (n)o | (q)uit]: n
# 
# [release-0.3.12 fc1629b] Version bump to 0.3.12 2 files changed, 12 insertions(+), 5 deletions(-)
# 
# To https://github.com/Rappster/bumpr * [new tag]         v0.3.12 -> v0.3.12
# $old
# [1] "0.3.11"
# 
# $new
# [1] "0.3.12"
```

#### Explanation what just happened

- The suggestion always takes the last version digit and increments it by one.
If you simply hit enter, this suggested version will be used. 

  However, you are of course free to provide any version you'd like as long as
it complies with the [semtatic versioning conventions](http://semver.org/).

- You are then asked if you want to update your `DESCRIPTION` file (currently updated fields: `Version` and `Date`).

- Then a last check before commencing with Git/GitHub related stuff is made.

- Based on the specification of your remote repository (**note that it is recommended to define it prior to running `bumpGitVersion()` but it can also be set by the function in case it has not been defined yet**) a new commit
is issued and **after** that a new tag corresponding to `v{new-version}` (e.g. `v0.3.11`) is created so **future** commits are automatically tagged with it.

- What also happens is that the files `CHANGES.md` and `NEWS.md` are updated
  as described in `?bumpr`.

- Before pushing to the remote (GitHub) repository, you are asked which form of HTTPS authentication you'd like to use: `PAT` ([Personal Access Token](https://github.com/blog/1509-personal-api-tokens)) or `Basic`.

  - if choosing `PAT` (recommended):
  
    If your PAT has not been set as an environment variable yet, you are asked to do so. Otherwise you are asked if you would like to review the currently set PAT. In this case you are also asked if you would like to change the PAT (in case the current PAT is wrong/outdated).
  
  - if choosing `Basic`
  
    Your credentials can either be looked in file `_netrc` in your `HOME` directory or you can typing it into the console. 

    This is also where argument `temp_credentials` comes into play: when `TRUE`
  the function makes sure that file `_netrc` is deleted after the version bump is
  finished.

  - If your remote repository does not require HTTPS authentication, simply hit `ENTER`.

- The function returns the old and new version as a `list` string. If
along the way something when wrong (wrong user input) or when you wanted to quit on purpose, the function returns `list()`.

-----

## Classes and constructors

Class `GitVersion.S3`

```
## Formal use //
bumpr::GitVersion.S3()

# $version
# character(0)
# 
# $remote_name
# character(0)
# 
# $remote_url
# character(0)
# 
# $user_email
# character(0)
# 
# $user_name
# character(0)
# 
# attr(,"class")
# [1] "GitVersion.S3" "list"   

## Informal use //
bumpr::GitVersion.S3("0.1.1")

# [1] "0.1.1"
# attr(,"class")
# [1] "GitVersion.S3" "character"    
```

Class `RPackageVersion.S3`

```
## Formal use //
bumpr::RPackageVersion.S3()

# $version
# character(0)
# 
# $lib
# character(0)
# 
# $path
# character(0)
# 
# attr(,"class")
# [1] "RPackageVersion.S3" "list"   

## Informal use //
bumpr::RPackageVersion.S3("0.1.1")

# [1] "0.1.1"
# attr(,"class")
# [1] "RPackageVersion.S3" "character"   
```

