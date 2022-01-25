#' performance_equiv function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B},
#'     test data, significance level \eqn{\alpha} and acceptable score
#'     degradation \eqn{\delta_B}. It checks whether the models perform
#'     equivalently on the test set and returns various figures.
#' @param model_a logistic regression model \eqn{M_A}
#' @param model_b logistic regression model \eqn{M_B}
#' @param test_data testing dataset
#' @param dv_index column number of the dependent variable
#' @param delta_B acceptable score degradation (defaults to 1.1)
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
#'   \item{\code{delta_B}}{Calculated equivalence parameter}
#'   \item{\code{p_value_l}}{P-value for \eqn{t_L}}
#'   \item{\code{p_value_u}}{P-value for \eqn{t_U}}
#' }
#' @export
#' @importFrom stats predict.glm var qt pt

performance_equiv <- function(model_a, model_b, test_data,
                              dv_index, delta_B = 1.1, alpha = 0.05) {
  m <- nrow(test_data)
  test_y <- test_data[, dv_index]
  eps_b <- delta_B^2
  pi_ac <- predict.glm(model_a, test_data, type = "response")
  pi_bc <- predict.glm(model_b, test_data, type = "response")
  bs_ac <- brier_score(test_y, pi_ac)
  bs_bc <- brier_score(test_y, pi_bc)
  b_ac <- (pi_ac - test_y)^2
  b_bc <- (pi_bc - test_y)^2
  d_l <- eps_b * b_bc - b_ac
  d_u <- b_bc - eps_b * b_ac
  test_stat_l <- mean(d_l) / sqrt(var(d_l))
  test_stat_u <- mean(d_u) / sqrt(var(d_u))
  equivalence_threshold <- qt(1 - alpha, df = (m - 1), lower.tail = T)
  bse_l <- (test_stat_l > equivalence_threshold)
  bse_u <- (test_stat_u < -equivalence_threshold)
  return(list(
    equivalence = (bse_l && bse_u),
    brier_score_ac = bs_ac,
    brier_score_bc = bs_bc,
    diff_sd_l = sd(d_l),
    diff_sd_u = sd(d_u),
    test_stat_l = test_stat_l,
    test_stat_u = test_stat_u,
    crit_val = equivalence_threshold,
    epsilon_B = eps_b,
    p_value_l = pchisq(test_stat_l, m - 1, lower.tail = FALSE),
    p_value_u = pchisq(test_stat_u, m - 1)
    )
  )
}
