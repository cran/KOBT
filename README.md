<!-- README.md is generated from README.Rmd. Please edit that file -->

KOBT
====

The goal of KOBT is to conduct variable selection in tree models with
false discovery rate control. The difference of SHAPs between original
and knockoff variables is used as the test statistic.

Example
-------

This is a basic example which shows you how to (1) generate knockoffs,
and (2) conduct variable selection in tree models with false discovery
rate control using KOBT.

``` r
## basic example code
library('KOBT')
# 1. Generate Knockoffs
set.seed(10)
X <- matrix(rnorm(100), nrow = 10)
Z <- generate.knockoff(X = X, type = "shrink", num = 2)
# 2. Conduct variable selection
beta <- rep(0, 10)
beta[1:5] <- 10
Y <- MASS::mvrnorm(n = 1, mu = X%*%beta, Sigma = diag(10))
result <- vector(mode = "list", length = length(Z))
for (i in 1:length(Z)) {
  x <- cbind(X, Z[[i]])
  dtrain <- xgboost::xgb.DMatrix(x, label = Y)
  fit.model <- xgboost::xgb.train(data = dtrain, nrounds = 2)
  result[[i]] <- importance.score(fit = fit.model, Y = Y, X = x)$shap
}
output <- matrix(unlist(result), ncol = length(result[[1]]), byrow = TRUE)
selected.index <- kobt.select(score = output)
```
