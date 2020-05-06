#' le_base_data function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B} and arranges their data for the equivalence functions.
#' @param M_A a logistic regression model
#' @param M_B a logistic regression model

le_base_data <- function(M_A, M_B){
  if((M_A$family$family != M_B$family$family) || (M_A$family$family != "binomial") || (M_B$family$family != "binomial")){
    stop("Mismatching Models")
  }
  le_data <- list()
  le_data$n_A <- nrow(M_A$data)
  le_data$n_B <- nrow(M_B$data)
  le_data$beta_A <- M_A$coefficients
  le_data$beta_B <- M_B$coefficients
  le_data$Sigma_beta_A <- summary(M_A)$cov.unscaled
  le_data$Sigma_beta_B <- summary(M_B)$cov.unscaled
  le_data$p <- length(le_data$beta_A)
  return(le_data)
}
