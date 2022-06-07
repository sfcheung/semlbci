skip("WIP")

library(lavaan)
mod <-
"
m ~ a*x
y ~ b*m
ab := a * b
"
fit_med <- sem(mod, simple_med, fixed.x = FALSE)
p_table <- parameterTable(fit_med)
p_table
lbci_med <- semlbci(fit_med)
confint(lbci_med)

lbci_med_std <- semlbci(fit_med, standardized = TRUE)
confint(lbci_med_std)


dat <- simple_med_mg
mod <-
"
m ~ x
y ~ m
"
fit_mg <- lavaan::sem(mod, simple_med_mg, fixed.x = FALSE, group = "gp")
lbci_med_mg <- semlbci(fit_mg, try_k_more_times = 5)
confint(lbci_med_mg)
