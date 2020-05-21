#' coef_vector_equivalence function
#'
#' This function takes two logistic regression models \eqn{M_A, M_B},
#'     test data, significance level \eqn{\alpha} and allowed flips ratio
#'     \eqn{r}. It checks whether the models produce equivalent log-odds for
#'     the given test set and returns various figures.
#' @param model_a logistic regression model \eqn{M_A}
#' @param model_b logistic regression model \eqn{M_B}
#' @param test_data testing dataset
#' @param alpha significance level \eqn{\alpha} (defaults to 0.05)
#' @param r ratio of allowed 'flips' (defaults to 0.1)
#' @return \describe{
#'   \item{\code{equivalence}}{Are models \eqn{M_A,M_B} producing equivalent
#'       log-odds for the given test data? (boolean)}
#'   \item{\code{test_statistic}}{The test statistic}
#'   \item{\code{critical_value}}{a level-\eqn{\alpha} critical value the test}
#'   \item{\code{xi_bar}}{Mean \eqn{\xi} value for the test}
#'   \item{\code{delta_theta}}{Calculated sensitivity level for the equivalence
#'       test}
#' }
#' @keywords theta equivalence, log-odds equivalence
#' @export
#' @importFrom stats predict.glm quantile sd qt pt

theta_equivalence <- function(model_a, model_b, test_data, r = 0.1,
                              alpha = 0.05) {
  n <- nrow(test_data)
  theta_ac <- predict.glm(model_a, test_data)
  theta_bc <- predict.glm(model_b, test_data)
  delta <- as.numeric(quantile(abs(theta_ac), r))

  xi <- abs(theta_ac - theta_bc)
  test_stat <- (mean(xi) - delta) / (sd(xi) / sqrt(n))
  crit_value <- qt(alpha, df = n - 1, lower.tail = FALSE)
  return(list(
              equivalence = (test_stat < crit_value),
              test_statistic = test_stat,
              critical_value = crit_value,
              xi_bar = mean(xi),
              delta_theta = delta
              ))
}
