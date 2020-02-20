#' @title Generate Knockoff Matrix
#' @description Generate different types of knockoff matrices given an original one.
#'
#' @param X An input original design matrix.
#' @param type The knockoff type to be generated. There are three choices available: (1) "shrink" for the shrink Gaussian knockoff;
#'             (2) "sparse" for the sparse Gaussian knockoff; and (3) "pc" for the pricial component knockoff.
#' @param num The number of knockoff matrices to be created.
#' @param num.comp The number of pricial components to be used for generating knockoff matrices, the default is 10.
#'
#' @return A list of created knockoff matrices.
#'
#' @export
#' @import knockoff
#' @import spcov
#' @import stats
#' @import Rdpack
#'
#' @examples
#' set.seed(10)
#' X <- matrix(rnorm(100), nrow = 10)
#' Z <- generate.knockoff(X = X, type = "shrink", num = 5)
#'
#' @references
#' \insertRef{barber2015controlling}{KOBT}
#' \insertRef{candes2018panning}{KOBT}
#' \insertRef{bien2011sparse}{KOBT}
generate.knockoff <- function(X, type, num, num.comp = 10) {
  result <- vector(mode = "list", length = num)

  if (type == "shrink") {
    for (i in 1:num) {
      result[[i]] <- knockoff::create.second_order(X = X, method = "sdp")
    }
  } else if (type == "sparse") {
    mu <- colMeans(X)
    S <- stats::cov(X)
    p <- ncol(X)
    step.size <- 100
    P <- matrix(1, p, p)
    diag(P) <- 0
    lam <- 0.06
    mm <- spcov::spcov(Sigma = diag(diag(S)), S = (S+0.1*diag(1,p)), lambda = lam*P, step.size = step.size)

    for (i in 1:num) {
      result[[i]] <- knockoff::create.gaussian(X = X, mu = mu, Sigma = mm$Sigma, method = "sdp")
    }
  } else if (type == "pc") {
    for (i in 1:num) {
      result[[i]] <- KOBT::create.pc.knockoff(X = X, pc.num = num.comp)
    }
  } else {
    stop("Invalid knockoff type!")
  }

  return(result)
}
