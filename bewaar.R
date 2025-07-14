

install.packages("usethis")
usethis::use_git_config(
  user.name  = "Bas Bakker",
  user.email = "bbakker@imf.org"
)


usethis::git_sitrep()

usethis::create_github_token()

usethis::use_package("AzureAuth")
usethis::use_package("rsdmx")
usethis::use_package("dplyr")
usethis::use_package("tibble")
usethis::use_package("stringr")
usethis::use_package("lubridate")
usethis::use_package("zoo")

usethis::use_package("usethis", type = "Suggests")
