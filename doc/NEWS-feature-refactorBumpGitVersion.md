# CHANGES IN bumpr FEATURE refactorBumpGitVersion

## NEW FEATURES

## BUG FIXES

## MAJOR CHANGES

- added: argument `project` in `bumpGitVersion()` and 
  `bump-GitVersion.S3-character-charcter-method`
- refactor: `bump-RPackageVersion.S3-character-charcter-method`
  - return value changed to `list(old = {old-vsn}, new = {new-vsn})` and
    `list()` when exiting
- refactor: `bump-GitVersion.S3-character-charcter-method`
  - now uses `bumpPackageVersion()` 
  - return value changed to `list(old = {old-vsn}, new = {new-vsn})` and
    `list()` when exiting
- refactor: documentation in `/R/bumpr.r`, `/R/bumpGitVersion()` and 
  `/R/bumpPackageVersion()`

## MINOR CHANGES

- refactor: `bumpPackageVersion()`
  - added arguments: `taken` and `desc_fields`
- refactored: `README.md`  

## TESTS

## MISC

