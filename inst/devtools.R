# Importing packages

devtools::use_package("dplyr")
devtools::use_package("lubridate")
devtools::use_package("readr")
devtools::use_package("magrittr")
devtools::use_package("lazyeval")

devtools::use_build_ignore(c("my_testing.R", "devtools.R"))
