# CHANGES IN bumpr FEATURE bumpPackageVersion

## NEW FEATURES

- added: `bumpPackageVersion()`

## BUG FIXES

- fixed #3 (not quitting on "update DESCRIPTION = no")

## MAJOR CHANGES

- added: `bumper-Bumpr.RPackage.S3-character-character-method`.
  - possible to explicitly set version numbers via `from` and `to`

## MINOR CHANGES

- renamed: `.getVersionInput()` --> `.askNewVersionNumber()`
- renamed: `.ask_updateDescriptionFile()` --> `askUpdateDescriptionFile`
- modified: added `skip("interactive only")` to interactive-only unit tests

## TESTS

- added: unit test for `bumper-Bumpr.RPackage.S3-character-character-method`
  in `/tests/testthat/test-bump.r` (context `bumpr-Bumpr.RPackage.S3`)

## MISC

