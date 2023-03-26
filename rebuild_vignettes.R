# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("semlbci.Rmd.original", output = "semlbci.Rmd")
knitr::knit("loglike.Rmd.original", output = "loglike.Rmd")
knitr::knit("technical_workflow.Rmd.original", output = "technical_workflow.Rmd")
knitr::knit("technical_searching_one_bound.Rmd.original", output = "technical_searching_one_bound.Rmd")
setwd(base_dir)
