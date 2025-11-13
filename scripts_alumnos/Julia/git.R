install.packages("usethis")
install.packages("gitcreds")

library(usethis)
library(gitcreds)

usethis::git_remotes()
use_git_config(user.name = "", user.email = "")

# HTTPs
usethis::create_github_token()
gitcreds::gitcreds_set()