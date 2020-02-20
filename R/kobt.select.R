#' @title Knockoff Variable Selection
#' @description Use knockoff to conduct variable selection with false discovery rarte control.
#'
#' @param score An n by 2p matrix of test statistics, which includes test statistics from n samples, p variables (first p columns), and p knockoff variables (last p columns).
#' @param fdr The targeted false discovery rate (FDR), the default value is 0.1.
#' @param type A charactor showing the type of calculated false discovery rate: (1) modified and (2) usual FDR, the default value is modified.
#'
#' @return Indices of selected columns/variables in the n by p original design matrix.
#'
#' @export
#' @import Rdpack
#'
#' @examples
#' set.seed(1010)
#' n <- 100
#' p <- 100
#' signal.num <- 20
#' W_left <- matrix(rnorm(n = n*signal.num, mean = 1, sd = 1), nrow = n)
#' W_right <- matrix(rnorm(n = n*(2*p-signal.num), mean = 0, sd = 1), nrow = n)
#' W <- cbind(W_left, W_right)
#' selected.index <- kobt.select(score = W)
#'
#' @references
#' \insertRef{candes2018panning}{KOBT}
kobt.select <- function(score, fdr = 0.1, type = "modified") {
  relative.score <- score[,c(1:(ncol(score)/2))] - score[,c((ncol(score)/2+1):ncol(score))]
  score.col.mean <- colMeans(relative.score)

  tmp <- sort(c(0, abs(score.col.mean)))
  if(type == "modified") {
    offset <- 0
  } else if(type == "usual") {
    offset <- 1
  } else {
    stop("Invalid FDR type!")
  }

  ratio <- sapply(tmp, function(t) (offset + sum(score.col.mean <= -t)) / max(1, sum(score.col.mean >= t)))
  selected <- which(ratio <= fdr)
  threshold <- ifelse(length(selected) > 0, tmp[selected[1]], Inf)
  index <- which(score.col.mean >= threshold)

  return(index)
}
