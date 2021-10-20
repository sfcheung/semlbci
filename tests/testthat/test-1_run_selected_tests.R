skip("Skip due to speed or other issues")
# To be tested in interactive sessions
library(testthat)
options(Ncpus = 9)

Sys.setenv("SEMLBCI_TEST_COMPREHENSIVE" = "TRUE")

#devtools::test()

Sys.unsetenv("SEMLBCI_TEST_COMPREHENSIVE")
