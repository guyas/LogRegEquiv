#' coef_vector_equivalence function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B},
#'     significance level \eqn{\alpha} and sensitivity level \eqn{\delta_B}.
#'     It checks whether the coefficient vectors are equivalent.
#' @param m_a logistic regression model \eqn{M_A}
#' @param m_b logistic regression model \eqn{M_B}
#' @param alpha significance level \eqn{\alpha} (defaults to 0.05)
#' @param delta equivalence sensitivity level \eqn{\delta_\beta} (defaults to 1)
#' @return \describe{
#'   \item{\code{test stat}}{Equivalence test statistic}
#'   \item{\code{critical value}}{level-\eqn{\alpha} critical value}
#'   \item{\code{ncp}}{non-centrality parameter}
#'   \item{\code{p_val}}{P-value for the test statistic}
#'   \item{\code{df}}{degrees of freedom}
#'   \item{\code{equivalence}}{are the coefficient vectors equivalent?
#'        (boolean)}
#' }
#' @keywords beta equivalence, vector equivalence
#' @export
#' @importFrom stats qchisq pchisq

coef_vector_equivalence <- function(m_a, m_b, alpha = 0.05, delta = 1) {
  le_data <- le_base_data(m_a, m_b)
  #vector difference
  q <- le_data$beta_a - le_data$beta_b
  #pooled sample covariance
  s_q <- le_data$sigma_beta_a + le_data$sigma_beta_b
  #Wald statistic
  test_stat <- t(q) %*% solve(s_q) %*% q
  #vector of delta
  if (length(delta) == length(q)) {
    d <- delta
  }
  else{
    d <- rep(delta, length(q))[seq_len(q)]
  }
  #non centrality parameter
  ncp <-  t(d) %*% solve(s_q) %*% d
  #critical value for the test (f.crit * sizing)
  c.alpha <- qchisq(p = alpha, df = le_data$p, ncp = ncp)
  return(list(`test stat` = test_stat,
              `critical value` = c.alpha,
              ncp = ncp,
              p_val = pchisq(q = test_stat, df = le_data$p, ncp = ncp),
              df = le_data$p,
              equivalence = (test_stat < c.alpha)))
}
