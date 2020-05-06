#' BrierScore function
#'
#' This function takes a predictions vector \eqn{\pi} and matching obsrevations \eqn{y} and returns the Brier score for the predictions.
#' @param pi the predictions vector
#' @param y the obsrevations vector
#' @keywords Brier
#' @export
#' @examples
#' pi_vec <- runif(10)
#' y_vec <- rbinom(10, 1, pi_vec)
#' BrierScore(pi_vec, y_vec)

BrierScore <- function(pi, y){
  if(length(y) != length(pi)){
    stop("mismatching vectors")
  }
  return(mean((y - pi)^2, na.rm = T))
}
