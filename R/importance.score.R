#' @title Importance Score
#' @description Generate SHAP (SHapley Additive exPlanations) and Saabas scores.
#'
#' @param fit A fitted object of class xgb.Booster.
#' @param Y A vector of responses.
#' @param X An input design matrix.
#'
#' @return A list of (1) shap, a vector of Hapley Additive exPlanations for each feature;
#'                   (2) saabas, a vector of an individualized heuristic feature attribution method, which can be considered as an approximation for shap.
#'
#' @export
#' @import xgboost
#' @import Rdpack
#'
#' @examples
#' set.seed(10)
#' X <- matrix(rnorm(100), nrow = 10)
#' Y <- matrix(rnorm(10), nrow = 10)
#' dtrain <- xgboost::xgb.DMatrix(X, label = Y)
#' fit.model <- xgboost::xgb.train(data = dtrain, nrounds = 5)
#' tmp <- importance.score(fit = fit.model, Y = Y, X = X)
#'
#' @references
#' \insertRef{candes2018panning}{KOBT}
#' \insertRef{chen2016xgboost}{KOBT}
#' \insertRef{lundberg2017unified}{KOBT}
importance.score <- function(fit, Y, X) {
  dtrain <- xgb.DMatrix(X, label = Y)

  shap_values <- predict(object = fit, newdata = dtrain, predcontrib = TRUE, approxcontrib = FALSE)
  shap_values <- shap_values[, -c(ncol(shap_values))] # Remove the last column for bias
  mean_abs_shap_values <- colMeans(abs(shap_values))

  saabas_values <- predict(object = fit, newdata = dtrain, predcontrib = TRUE, approxcontrib = TRUE)
  saabas_values <- saabas_values[, -c(ncol(saabas_values))] # Remove the last column for bias
  mean_abs_saabas_values <- colMeans(abs(saabas_values))

  result <- list("shap" = mean_abs_shap_values, "saabas" = mean_abs_saabas_values)
  return (result)
}
