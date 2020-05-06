#' brier_equivalence function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B},
#'     significance level \eqn{\alpha} and sensitivity level \eqn{\delta_B}.
#'     It checks whether the models perform equivalently on each of their
#'     training datasets and returns various figures.
#' @param m_a logistic regression model \eqn{M_A}
#' @param m_b logistic regression model \eqn{M_B}
#' @param alpha significance level \eqn{\alpha} (defaults to 0.05)
#' @param delta equivalence sensitivity level \eqn{\delta_B} (defaults to 1)
#' @return the \emph{AB} notation refers to the fit of model A to the data of
#'      model B (and \emph{BA} for vice versa) \describe{
#'   \item{\code{bs_mat}}{Brier scores matrix}
#'   \item{\code{bs_ab_equiv}}{Are models \eqn{M_A,M_B} producing equivalent
#'       Brier scores for \eqn{M_B}'s training data? (boolean)}
#'   \item{\code{bs_ba_equiv}}{Are models \eqn{M_A,M_B} producing equivalent
#'       Brier scores for \eqn{M_A}'s training data? (boolean)}
#'   \item{\code{test_stat_ab_l}}{\eqn{t_L} for the AB test}
#'   \item{\code{test_stat_ab_u}}{\eqn{t_U} for the AB test}
#'   \item{\code{test_stat_ba_l}}{\eqn{t_L} for the BA test}
#'   \item{\code{test_stat_ba_u}}{\eqn{t_U} for the BA test}
#'   \item{\code{crit_ab}}{level-\eqn{\alpha} critical value the AB test}
#'   \item{\code{crit_ba}}{level-\eqn{\alpha} critical value the BA test}
#'
#' }
#' @keywords brier equivalence
#' @export

brier_equivalence <- function(m_a, m_b, alpha = 0.05, delta = 1) {
  le_data <- le_base_data(m_a, m_b)

  pi_aa <- predict.glm(m_a, m_a$data, type = "response")
  pi_ab <- predict.glm(m_a, m_b$data, type = "response")
  pi_ba <- predict.glm(m_b, m_a$data, type = "response")
  pi_bb <- predict.glm(m_b, m_b$data, type = "response")
  brier_scores_mat <- matrix(0, ncol = 2, nrow = 2)
  colnames(brier_scores_mat) <- c("model A", "model B")
  rownames(brier_scores_mat) <- c("data A", "data B")
  brier_scores_mat["data A", "model A"] <- brier_score(m_a$y, pi_aa)
  brier_scores_mat["data A", "model B"] <- brier_score(m_a$y, pi_ba)
  brier_scores_mat["data B", "model A"] <- brier_score(m_b$y, pi_ab)
  brier_scores_mat["data B", "model B"] <- brier_score(m_b$y, pi_bb)

  b_aa <- (pi_aa - m_a$y)^2
  b_ab <- (pi_ab - m_b$y)^2
  b_ba <- (pi_ba - m_a$y)^2
  b_bb <- (pi_bb - m_b$y)^2

  d_ab <- b_ab - b_bb
  d_ba <- b_ba - b_aa

  test_stat_ab_l <- sqrt(le_data$n_b) * (mean(d_ab) + delta) / sqrt(var(d_ab))
  test_stat_ab_u <- sqrt(le_data$n_b) * (mean(d_ab) - delta) / sqrt(var(d_ab))
  test_stat_ba_l <- sqrt(le_data$n_a) * (mean(d_ba) + delta) / sqrt(var(d_ba))
  test_stat_ba_u <- sqrt(le_data$n_a) * (mean(d_ba) - delta) / sqrt(var(d_ba))

  thresh_for_equiv_ab <- qt(1 - alpha, df = (le_data$n_b - 1), lower.tail = T)
  thresh_for_equiv_ba <- qt(1 - alpha, df = (le_data$n_a - 1), lower.tail = T)

  bs_ab_left <- (thresh_for_equiv_ab < test_stat_ab_l)
  bs_ab_right <- (test_stat_ab_u < -thresh_for_equiv_ab)
  bs_ba_left <- (thresh_for_equiv_ba < test_stat_ba_l)
  bs_ba_right <- (test_stat_ba_u < -thresh_for_equiv_ba)

  return(list(bs_mat = brier_scores_mat,
              bs_ab_equiv = (bs_ab_left && bs_ab_right),
              bs_ba_equiv = (bs_ba_left && bs_ba_right),
              test_stat_ab_l = test_stat_ab_l,
              test_stat_ab_u = test_stat_ab_u,
              test_stat_ba_l = test_stat_ba_l,
              test_stat_ba_u = test_stat_ba_u,
              crit_ab = thresh_for_equiv_ab,
              crit_ba = thresh_for_equiv_ba)
  )
}
