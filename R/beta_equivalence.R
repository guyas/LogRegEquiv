#' beta_equivalence function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B},
#'     sensitivity level \eqn{\delta_\beta} and significance level \eqn{\alpha}.
#'     It checks whether the coefficient vectors are equivalent.
#' @param model_a logistic regression model \eqn{M_A}
#' @param model_b logistic regression model \eqn{M_B}
#' @param delta equivalence sensitivity level \eqn{\delta_\beta}. This could
#'     either be a scalar or a vector with length matching the number of
#'     ceofficients.
#' @param alpha significance level \eqn{\alpha}
#' @return \describe{
#'   \item{\code{equivalence}}{are the coefficient vectors equivalent?
#'        (boolean)}
#'   \item{\code{test_statistic}}{Equivalence test statistic}
#'   \item{\code{critical value}}{a level-\eqn{\alpha} critical value}
#'   \item{\code{ncp}}{non-centrality parameter}
#'   \item{\code{p_value}}{P-value}
#' }
#' @keywords beta equivalence, vector equivalence
#' @export
#' @importFrom stats qchisq pchisq

beta_equivalence <- function(model_a, model_b, delta, alpha) {
  if ((model_a$family$family != model_b$family$family) ||
      (model_a$family$family != "binomial") ||
      (model_b$family$family != "binomial")) {
    stop("Mismatching model families")
  }
  if (model_a$formula != model_b$formula) {
    stop("Mismatching regression formulae")
  }

  if (length(model_a$coefficients) != length(model_b$coefficients)) {
    stop("Mismatching covariates")
  }

  if (length(which(names(model_a$coefficients) != names(model_b$coefficients)))
      > 0) {
    stop("Mismatching covariates")
  }

  coef_a <- model_a$coefficients
  coef_b <- model_b$coefficients
  p <- length(coef_a)
  #vector of delta
  if (length(delta) == p) {
    d <- delta
  }
  else{
    if (length(delta) == 1) {
      d <- rep(delta, p)
    } else {
      stop("Mismatching delta vector")
    }
  }

  sigma_coef_b <- summary(model_b)$cov.unscaled
  sigma_coef_a <- summary(model_a)$cov.unscaled

  #vector difference
  coef_vec_diff <- coef_a - coef_b
  #pooled sample covariance
  cov_mat <- sigma_coef_a + sigma_coef_b
  #Wald statistic
  test_stat <- t(coef_vec_diff) %*% solve(cov_mat) %*% coef_vec_diff
  #non centrality parameter
  ncp <-  t(d) %*% solve(cov_mat) %*% d
  #critical value for the test (f.crit * sizing)
  c_alpha <- qchisq(p = alpha, df = p, ncp = ncp)
  return(list(equivalence = (test_stat < c_alpha),
              test_statistic = test_stat,
              critical_value = c_alpha,
              ncp = ncp,
              p_value = pchisq(test_stat, p, ncp)
            )
        )
}
