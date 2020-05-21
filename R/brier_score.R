#' brier_score function
#'
#' This function takes a obsrevations vector \eqn{y} and matching
#'      predictions vector \eqn{\pi}. It returns the Brier score for the
#'      predictions.
#' @param pi the predictions vector
#' @param y the obsrevations vector
#' @return The Brier score \eqn{\frac{1}{N}\sum_{i=1}^{N}{(y_i-\pi_i)^2}}
#' @keywords Brier
#' @export
#' @examples
#' brier_score(rbinom(10,1,seq(0.1, 1, 0.1)), seq(0.1, 1, 0.1))

brier_score <- function(y, pi) {
  if (length(y) != length(pi)) {
    stop("mismatching vectors")
  }
  if (sum((y == 1) | (y == 0)) < length(y)) {
    stop("illegal y vector")
  }
  if (sum((pi <= 1) & (pi >= 0)) < length(pi)) {
    stop("illegal pi vector")
  }
  return(mean((y - pi)^2, na.rm = T))
}
