\dontrun{

## Informal use (intended mainly for rapid prototyping) //
## Takes *any* object and simply changes the class attributes
SystemState.S3(TRUE)  

## Formal use (explicitly using 'fields') //
SystemState.S3()
SystemState.S3(
  ask_authentication = TRUE,
  branch = "master",
  cmd_user_email = "git config --global user.email",
  cmd_user_name = "git config --global user.name",
  description_old = as.list(read.dcf("DESCRIPTION")[1,]),
  git_tag = "v1.1.2",
  git_user_email = "janko.thyson@rappster.de",
  git_user_name = "Janko Thyson",
  global_or_local = "global",
  pat_or_basic = "pat",
  path_netrc = file.path(Sys.getenv("HOME"), "_netrc"),
  path_netrc_tmp = file.path(Sys.getenv("HOME"), "_netrc_0"),
  quit = FALSE,
  remote = "origin",
  remote_all = list(origin = character()),
  remote_name = "origin",
  remote_url = "https://github.com/Rappster/bumpr",
  temp_credentials = FALSE,
  what = bumpr::GitVersion.S3()
)

## Recommended: inlcude namespace //
## Regardless if you plan on using this class in an informal or formal way
bumpr::SystemState.S3()

}
