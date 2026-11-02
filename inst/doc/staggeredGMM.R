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

## ----jtest-cov----------------------------------------------------------------
gmm_j_test(fit_cov)

## ----jtest-full---------------------------------------------------------------
gmm_j_test(fit, type = "full")

## ----pitfall-always, error = TRUE---------------------------------------------
try({
bad <- sim_panel
bad$cohort[bad$cohort == 10] <- 1
gmm_staggered(bad, yname = "y", tname = "year",
              idname = "unit_id", gname = "cohort")
})

## ----pitfall-never------------------------------------------------------------
fit_nyt <- gmm_staggered(sim_panel[sim_panel$cohort != 0, ], yname = "y",
                         tname = "year", idname = "unit_id",
                         gname = "cohort")
table(fit_nyt$catt$identified)
fit_nyt$aggregate$CW$identified_weight_share

## ----citation, eval = FALSE---------------------------------------------------
# citation("staggeredGMM")

