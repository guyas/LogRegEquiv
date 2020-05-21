#' coef_vector_equivalence function
#'
#' This function takes two datasets \eqn{X_A, X_B}, regression formula,
#'     significance level \eqn{\alpha} and sensitivity level
#'     \eqn{\delta_\beta} (either vector or scalar). It builds a logistic
#'     regression model for each of the datasets and then checks whether the
#'     obtained coefficient vectors are equivalent, using the
#'     \code{beta_equivalence} function.
#' @param data_a logistic regression model \eqn{M_A}
#' @param data_b logistic regression model \eqn{M_B}
#' @param formula logistic regression formula
#' @param delta equivalence sensitivity level \eqn{\delta_\beta}
#' @param alpha significance level \eqn{\alpha} (defaults to 0.05)
#' @return \describe{
#'   \item{\code{critical value}}{level-\eqn{\alpha} critical value}
#' }
#' @keywords coefficients vector equivalence
#' @export
#' @importFrom stats glm binomial as.formula

coef_vector_equivalence <- function(data_a, data_b, formula, delta,
                                    alpha = 0.05) {
  model_a <- glm(formula = as.formula(formula),
                 family = binomial(link = "logit"),
                 data = data_a
  )

  model_b <- glm(formula = as.formula(formula),
                 family = binomial(link = "logit"),
                 data = data_b
  )
  return(beta_equivalence(model_a, model_b, delta, alpha))
}
