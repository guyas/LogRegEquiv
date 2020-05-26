#' Sigmoid function
#'
#' This function takes a number \eqn{\theta} and returns its
#'     respective sigmoid probability \eqn{\frac{e^{theta}}{1+e^{theta}}}.
#'     This is used in logistic regression to model \eqn{P(y=1|x)}.
#' @param theta the linear predictor
#' @return the sigmoid probability
#' @keywords sigmoid
#' @export
#' @examples
#' sigmoid(0)

sigmoid <- function(theta) {
  if (!(is.numeric(theta))) {
    stop("non-numeric input")
  }
  return(exp(theta) / (1 + exp(theta)))
  }
