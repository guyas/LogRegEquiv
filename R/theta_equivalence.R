#' coef_vector_equivalence function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B},
#'     significance level \eqn{\alpha} and sensitivity level \eqn{\delta_B}.
#'     It checks whether the log-odds vectors are equivalent.
#' @param m_a logistic regression model \eqn{M_A}
#' @param m_b logistic regression model \eqn{M_B}
#' @param alpha significance level \eqn{\alpha} (defaults to 0.05)
#' @param delta equivalence sensitivity level \eqn{\delta_\beta} (defaults to 1)
#' @param r ratio of allowed 'flips' (defaults to 0)
#' @return the \emph{AB} notation refers to the fit of model A to the data of
#'      model B (and \emph{BA} for vice versa) \describe{
#'   \item{\code{ab_equiv}}{Are models \eqn{M_A,M_B} producing equivalent
#'       log-odds for \eqn{M_B}'s training data? (boolean)}
#'   \item{\code{ba_equiv}}{Are models \eqn{M_A,M_B} producing equivalent
#'       log-odds for \eqn{M_A}'s training data? (boolean)}
#'   \item{\code{test_stat_ab}}{The AB test statistic}
#'   \item{\code{test_stat_ba}}{The BA test statistic}
#'   \item{\code{crit_ab}}{level-\eqn{\alpha} critical value the AB test}
#'   \item{\code{crit_ba}}{level-\eqn{\alpha} critical value the BA test}
#'   \item{\code{xi_bar_ab}}{Mean \eqn{\xi} value for the AB test}
#'   \item{\code{xi_bar_ba}}{Mean \eqn{\xi} value for the BA test}
#'   \item{\code{delta_ab}}{Sensitivity level for the AB equivalence test
#'       (useful in case of indicating \code{r} value)}
#'   \item{\code{delta_ba}}{Sensitivity level for the BA equivalence test
#'       (useful in case of indicating \code{r} value)}
#'   \item{\code{p_val_ab}}{P-value for the AB test statistic}
#'   \item{\code{p_val_ba}}{P-value for the BA test statistic}
#' }
#' @keywords beta equivalence, vector equivalence
#' @export

theta_equivalence <- function(m_a, m_b, alpha = 0.05, delta = 1, r = 0) {
  #A's predictions on self
  theta_aa <- predict.glm(m_a, m_a$data)
  #A's predictions on B
  theta_ab <- predict.glm(m_a, m_b$data)
  #B's predictions on A
  theta_ba <- predict.glm(m_b, m_a$data)
  #B's predictions on self
  theta_bb <- predict.glm(m_b, m_b$data)
  if ((r > 0) && (r < 1)) {
    delta_ab <- as.numeric(quantile(abs(theta_bb), r))
    delta_ba <- as.numeric(quantile(abs(theta_aa), r))
  }else{
    delta_ab <- delta
    delta_ba <- delta
  }

  #does A fit B?
  xi_ab <- abs(theta_ab - theta_bb)
  test_stat_ab <- (mean(xi_ab) - delta_ab) / (sd(xi_ab) / sqrt(nrow(m_b$data)))
  df_ab <- nrow(m_b$data) - 1
  crit_ab <- qt(alpha, df_ab, lower.tail = FALSE)

  #does B fit A?
  xi_ba <- abs(theta_ba - theta_aa)
  test_stat_ba <- (mean(xi_ba) - delta_ba) / (sd(xi_ba) / sqrt(nrow(m_a$data)))
  df_ba <- nrow(m_a$data) - 1
  crit_ba <- qt(alpha, df_ba, lower.tail = FALSE)

  return(list(ab_equiv = ((test_stat_ab) < (crit_ab)),
              ba_equiv = ((test_stat_ba) < (crit_ba)),
              test_stat_ab = test_stat_ab,
              test_stat_ba = test_stat_ba,
              crit_ab = (crit_ab),
              crit_ba = (crit_ba),
              xi_bar_ab = mean(xi_ab),
              xi_bar_ba = mean(xi_ba),
              delta_ab = delta_ab,
              delta_ba = delta_ba,
              p_val_ab = pt(q = (test_stat_ab), df = df_ab, lower.tail = F),
              p_val_ba = pt(q = (test_stat_ba), df = df_ba, lower.tail = F)
  ))
}
