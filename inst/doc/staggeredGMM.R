## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(staggeredGMM)

## ----basic--------------------------------------------------------------------
fit <- gmm_staggered(sim_panel, yname = "y", tname = "year",
                     idname = "unit_id", gname = "cohort")
fit

## ----catt---------------------------------------------------------------------
head(fit$catt)

## ----methods------------------------------------------------------------------
head(coef(fit))
head(confint(fit))

## ----aggregates---------------------------------------------------------------
c(CW = fit$aggregate$CW$estimate, EW = fit$aggregate$EW$estimate)

## ----weighting----------------------------------------------------------------
fit_ht <- gmm_staggered(sim_panel, yname = "y", tname = "year",
                        idname = "unit_id", gname = "cohort",
                        weighting = "cohort_toeplitz")
c(pooled = fit$aggregate$CW$estimate, cohort = fit_ht$aggregate$CW$estimate)

## ----convergence--------------------------------------------------------------
unlist(fit$convergence[c("converged", "solve_ok", "termination", "n_iter")])

## ----unrestricted-------------------------------------------------------------
fit_u <- gmm_staggered(sim_panel, yname = "y", tname = "year",
                       idname = "unit_id", gname = "cohort",
                       weighting = "unrestricted")

## ----unbalanced---------------------------------------------------------------
gappy <- sim_panel
set.seed(1)
gappy$y[sample(nrow(gappy), 100)] <- NA
fit_gap <- gmm_staggered(gappy, yname = "y", tname = "year",
                         idname = "unit_id", gname = "cohort")
c(balanced = fit$aggregate$CW$estimate,
  unbalanced = fit_gap$aggregate$CW$estimate)

## ----partial------------------------------------------------------------------
names(fit$aggregate$CW)

## ----covar--------------------------------------------------------------------
fit_cov <- gmm_staggered(sim_panel, yname = "y", tname = "year",
                         idname = "unit_id", gname = "cohort",
                         covar = c("x1", "x2"))
fit_cov$aggregate$CW$estimate

## ----jtest--------------------------------------------------------------------
gmm_j_test(fit)

## ----jtest-full---------------------------------------------------------------
gmm_j_test(fit, type = "full")

## ----beck-pitfall, error = TRUE-----------------------------------------------
try({
gmm_staggered(beck_banks, yname = "ln_gini", tname = "wrkyr",
              idname = "state", gname = "branch_reform")
})

## ----beck-fixed---------------------------------------------------------------
dat <- beck_banks[beck_banks$branch_reform > 1976, ]
fit_beck <- gmm_staggered(dat, yname = "ln_gini", tname = "wrkyr",
                          idname = "state", gname = "branch_reform")
fit_beck

## ----beck-identified----------------------------------------------------------
table(fit_beck$catt$identified)
fit_beck$aggregate$CW$identified_weight_share

## ----citation, eval = FALSE---------------------------------------------------
# citation("staggeredGMM")

