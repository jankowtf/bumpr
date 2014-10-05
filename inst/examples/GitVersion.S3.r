\dontrun{

## Informal use (intended mainly for rapid prototyping) //
## Takes *any* object and simply changes the class attributes
GitVersion.S3("0.1.1")  
GitVersion.S3(rep("0.1.1", 3))  
GitVersion.S3(as.list(rep("0.1.1", 3)))
GitVersion.S3(TRUE)  

## Formal use (explicitly using 'fields') //
GitVersion.S3()
GitVersion.S3(
  version = "0.1.1",
  remote_name = "origin",
  remote_url = "https://github.com/Rappster/bumpr",
  user_email = "janko.thyson@rappster.de",
  user_name = "Janko Thyson",
  home = Sys.getenv("HOME")
)

## Recommended: inlcude namespace //
## Regardless if you plan on using this class in an informal or formal way
bumpr::GitVersion.S3("0.1.1")

}
