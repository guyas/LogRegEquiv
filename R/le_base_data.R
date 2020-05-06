#' le_base_data function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B} and
#'      arranges their data for the equivalence functions.
#' @param m_a a logistic regression model
#' @param m_b a logistic regression model

le_base_data <- function(m_a, m_b) {
  if ((m_a$family$family != m_b$family$family) ||
      (m_a$family$family != "binomial") ||
      (m_b$family$family != "binomial")) {
    stop("Mismatching Models")
  }
  le_data <- list()
  le_data$n_a <- nrow(m_a$data)
  le_data$n_b <- nrow(m_b$data)
  le_data$beta_a <- m_a$coefficients
  le_data$beta_b <- m_b$coefficients
  le_data$sigma_beta_a <- summary(m_a)$cov.unscaled
  le_data$sigma_beta_b <- summary(m_b)$cov.unscaled
  le_data$p <- length(le_data$beta_a)
  return(le_data)
}
