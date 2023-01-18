skip("To be run in an interactive session")
# To be tested in interactive sessions
library(testthat)
options(Ncpus = 13)

Sys.setenv("SEMLBCI_TEST_COMPREHENSIVE" = "TRUE")

# devtools::test()

Sys.unsetenv("SEMLBCI_TEST_COMPREHENSIVE")
