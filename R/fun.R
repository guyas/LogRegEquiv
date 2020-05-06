#' Sigmoid function
#'
#' This function takes a number \eqn{\theta\in\mathbb{R}} and returns its respective probability in \eqn{[0,1]}.
#' @param theta the linear predictor
#' @keywords sigmoid
#' @export
#' @examples
#' sigmoid(0)

sigmoid <- function(theta) {return(exp(theta)/(1+exp(theta)))}
