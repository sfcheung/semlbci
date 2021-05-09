skip("To be tested in interactive sessions only due to scoping issues")

# Not yet work.

library(testthat)

test_list <-
  c(dir("./tests/testthat/", pattern = "*_std*.R", full.names = TRUE),
    dir("./tests/testthat/", pattern = "*_std_*.R", full.names = TRUE))

Sys.setenv(run_interactive = TRUE)
out <- lapply(test_list, test_file, env = .GlobalEnv)
Sys.unsetenv("run_interactive")