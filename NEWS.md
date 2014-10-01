# CHANGES IN VERSION 0.3.9

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.8

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

- fixed #9: `NEWS.md` only appended when actually necessary (actual new version bump)
- fixed #10: user is now explicitly asked if Personal Authentication Token (PAT) or basic HTTPS authentication (user name and password) should be used.

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.7

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

- fixes #7: now possible to explicitly set remote repository name
- fixes #8: ability to choose between local and global git user information

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.6

## NEW FEATURES

- added: argument `pat` to specify personal authentication tokens in `bump()`.
  The token needs to be set via system environment variable `GITHUB_PAT`. This closed #1.

## BUG FIXES

- Fixed bugs #5

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.6

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.5

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.5

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.5

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.4

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.3

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN VERSION 0.3.2

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

## MINOR CHANGES

## MISC

-----

# CHANGES IN bumpr VERSION 0.3.1

## NEW FEATURES

- Supports now any Git remote repository and is thus no longer bound to 
  working with GitHub repositories only
- Added functionality to fine tune the interactive management of git repositories,
  especially with respect to the "early stages" (no initial commit yet, 
  no remote repository set yet, no `.gitignore` file yet, no branches in 
  remote repository etc.).
- Implemented some additional sanity checks before bumping the version

## BUG FIXES

- fixed some issues that arise at certain repository constellations
- fixed bug #2 (`NEWS.md` file now appended)

## MAJOR CHANGES

- started to factor out user input and git related code into own functions.
  Currently, they are still "private" in the sense that they are defined 
  withing a method of `bump()`, but I will probably factor them out into 
  own package-wide functions to increase the chances of being able to reuse
  code.

## MINOR CHANGES

- Updated documentation accordingly

## MISC

-----
