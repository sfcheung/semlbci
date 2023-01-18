skip("To be run in an interactive session")
# To be tested in interactive sessions only due to scoping or speed issues
# Not yet work. Passed in interactive sessions but failed in test_file().

library(testthat)

test_list <-
  c(dir("./tests/testthat/",
        pattern = glob2rx("*_i_std*.R"),
        full.names = TRUE),
    dir("./tests/testthat/",
        pattern = glob2rx("*_i_std_*.R"),
        full.names = TRUE))

Sys.setenv("SEMLBCI_TEST_SLOW" = "true")
Sys.setenv("NOT_CRAN" = "true")

for (i in test_list) {
    test_file(i)
  }

Sys.unsetenv("SEMLBCI_TEST_SLOW" = "")
Sys.unsetenv("NOT_CRAN")