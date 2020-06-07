#' brier_score function
#'
#' This function takes a obsrevations vector \eqn{y} and matching
#'      predictions vector \eqn{\pi}. It returns the Brier score for the
#'      predictions. Unless specified otherwise, input containing NAs will
#'      result with an NA.
#' @param pi the predictions vector
#' @param y the obsrevations vector
#' @param na.rm ignore NA? (optional)
#' @return The Brier score \eqn{\frac{1}{N}\sum_{i=1}^{N}{(y_i-\pi_i)^2}}
#' @keywords Brier
#' @export
#' @examples
#' brier_score(rbinom(10,1,seq(0.1, 1, 0.1)), seq(0.1, 1, 0.1))

brier_score <- function(y, pi, na.rm = FALSE) {
  if (((sum(is.na(y)) > 0) || (sum(is.na(pi)) > 0)) && (na.rm == FALSE)) {
    return(NA)
  }
  if (length(y) != length(pi)) {
    stop("mismatching vectors")
  }
  if (na.rm) {
    pi2 <- pi[!is.na(pi)]
    if (sum((pi2 <= 1) & (pi2 >= 0)) < length(pi2)) {
      stop("illegal pi vector")
    }
    y2 <- y[!is.na(y)]
    if (sum((y2 == 1) | (y2 == 0)) < length(y2)) {
      stop("illegal y vector")
    }
  } else {
    if (sum((y == 1) | (y == 0)) < length(y)) {
      stop("illegal y vector")
    }
    if (sum((pi <= 1) & (pi >= 0)) < length(pi)) {
      stop("illegal pi vector")
    }
  }
  return(mean((y - pi)^2, na.rm = na.rm))
}
