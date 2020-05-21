#' coef_vector_equivalence function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B},
#'     significance level \eqn{\alpha} and sensitivity level \eqn{\delta_B}.
#'     It checks whether the coefficient vectors are equivalent.
#' @param data_a logistic regression model \eqn{M_A}
#' @param data_b logistic regression model \eqn{M_B}
#' @param formula logistic regression formula
#' @param delta equivalence sensitivity level \eqn{\delta_\beta}
#' @param alpha significance level \eqn{\alpha} (defaults to 0.05)
#' @return \describe{
#'   \item{\code{critical value}}{level-\eqn{\alpha} critical value}
#' }
#' @keywords beta equivalence, vector equivalence
#' @export
#' @importFrom stats qchisq pchisq

coef_vector_equivalence <- function(data_a, data_b, formula, delta,
                                    alpha = 0.05) {

}
