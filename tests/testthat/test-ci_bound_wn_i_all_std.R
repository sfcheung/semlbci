skip_if(Sys.getenv("SEMLBCI_TEST_SLOW") == "",
        "Skip due to speed or other issues")
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

for (i in test_list) {
    test_file(i)
  }

Sys.setenv("SEMLBCI_TEST_SLOW" = "")