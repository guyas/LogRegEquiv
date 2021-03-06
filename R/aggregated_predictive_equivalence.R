#' aggregated_predictive_equivalence function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B},
#'     test data, significance level \eqn{\alpha} and sensitivity level
#'     \eqn{\delta_B}. It checks whether the models perform equivalently on the
#'     test set and returns various figures.
#' @param model_a logistic regression model \eqn{M_A}
#' @param model_b logistic regression model \eqn{M_B}
#' @param test_data testing dataset
#' @param dv_index column number of the dependent variable
#' @param t acceptable tolerance level (defaults to 0.1)
#' @param alpha significance level \eqn{\alpha} (defaults to 0.05)
#' @return \describe{
#'   \item{\code{equivalence}}{Are models \eqn{M_A,M_B} producing equivalent
#'       Brier scores for the given test data? (boolean)}
#'   \item{\code{brier_score_ac}}{\eqn{M_A} Brier score on the testing data}
#'   \item{\code{brier_score_bc}}{\eqn{M_B} Brier score on the testing data}
#'   \item{\code{diff_sd}}{SD of the Brier differences}
#'   \item{\code{test_stat_l}}{\eqn{t_L} equivalence boundary for the test}
#'   \item{\code{test_stat_u}}{\eqn{t_U} equivalence boundary for the test}
#'   \item{\code{crit_val}}{a level-\eqn{\alpha} critical value for the test}
#'   \item{\code{t}}{Calculated equivalence parameter}
#'   \item{\code{p_value_l}}{P-value for \eqn{t_L}}
#'   \item{\code{p_value_u}}{P-value for \eqn{t_U}}
#' }
#' @keywords brier equivalence
#' @export
#' @importFrom stats predict.glm var qt pt

aggregated_predictive_equivalence <- function(model_a, model_b, test_data,
                              dv_index, t = 0.1, alpha = 0.05) {
  m <- nrow(test_data)
  test_y <- test_data[, dv_index]
  pi_ac <- predict.glm(model_a, test_data, type = "response")
  pi_bc <- predict.glm(model_b, test_data, type = "response")
  bs_ac <- brier_score(test_y, pi_ac)
  bs_bc <- brier_score(test_y, pi_bc)
  b_ac <- (pi_ac - test_y)^2
  b_bc <- (pi_bc - test_y)^2
  d <- b_ac - b_bc
  b_abs <- abs(test_y - pi_ac)
  delta <- (2 * t) * mean(b_abs) - t^2
  test_stat_l <- sqrt(m) * (mean(d) + delta) / sqrt(var(d))
  test_stat_u <- sqrt(m) * (mean(d) - delta) / sqrt(var(d))
  equivalence_threshold <- qt(1 - alpha, df = (m - 1), lower.tail = T)
  bse_left <- (equivalence_threshold < test_stat_l)
  bse_right <- (test_stat_u < -equivalence_threshold)
  return(list(
    equivalence = (bse_left && bse_right),
    brier_score_ac = bs_ac,
    brier_score_bc = bs_bc,
    diff_sd = sd(d),
    test_stat_l = test_stat_l,
    test_stat_u = test_stat_u,
    crit_val = equivalence_threshold,
    delta_B = delta,
    p_value_l = pchisq(test_stat_l, m - 1, lower.tail = FALSE),
    p_value_u = pchisq(test_stat_u, m - 1)
    )
  )
}
