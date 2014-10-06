\dontrun{

## Careful! 
## This simulates the situation when a version bump is rolled back due to 
## 'bumpGitVersion()' being aborted or in case it failed.
## This changes the version in your DESCRIPTION file and you should make sure
## that you change it back to the correct one after trying out the function
  
vsn_before_reset <- as.list(read.dcf("DESCRIPTION")[1,])$Version

resetPackageVersion(vsn = "0.1")
vsn_after_reset <- as.list(read.dcf("DESCRIPTION")[1,])$Version

## Cleanup //
resetPackageVersion(vsn = vsn_before_reset)

}
