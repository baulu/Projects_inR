

# Creating a new Project(GITHub-First approach) and connecting to GIT
library(usethis)
library(gitcreds)
usethis::use_git()
usethis::create_github_token()
usethis::use_github()
gitcreds::gitcreds_set() 
