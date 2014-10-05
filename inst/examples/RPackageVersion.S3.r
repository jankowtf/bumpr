\dontrun{

## Informal use (intended mainly for rapid prototyping) //
## Takes *any* object and simply changes the class attributes
RPackageVersion.S3("0.1.1")  
RPackageVersion.S3(rep("0.1.1", 3))  
RPackageVersion.S3(as.list(rep("0.1.1", 3)))
RPackageVersion.S3(TRUE)  

## Formal use (explicitly using 'fields') //
RPackageVersion.S3()
RPackageVersion.S3(
  version = "0.1.1",
  lib = .libPaths()[1],
  path = getwd()
)

## Recommended: inlcude namespace //
## Regardless if you plan on using this class in an informal or formal way
bumpr::RPackageVersion.S3("0.1.1")

}
