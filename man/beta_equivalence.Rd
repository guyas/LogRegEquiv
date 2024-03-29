% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beta_equivalence.R
\name{beta_equivalence}
\alias{beta_equivalence}
\title{beta_equivalence function}
\usage{
beta_equivalence(model_a, model_b, delta, alpha)
}
\arguments{
\item{model_a}{logistic regression model \eqn{M_A}}

\item{model_b}{logistic regression model \eqn{M_B}}

\item{delta}{equivalence sensitivity level \eqn{\delta_\beta}. This could
either be a scalar or a vector with length matching the number of
coefficients.}

\item{alpha}{significance level \eqn{\alpha}}
}
\value{
\describe{
  \item{\code{equivalence}}{are the coefficient vectors equivalent?
       (boolean)}
  \item{\code{test_statistic}}{Equivalence test statistic}
  \item{\code{critical value}}{a level-\eqn{\alpha} critical value}
  \item{\code{ncp}}{non-centrality parameter}
  \item{\code{p_value}}{P-value}
}
}
\description{
This function takes two logistic regression models \eqn{M_A, M_B},
    sensitivity level \eqn{\delta_\beta} and significance level \eqn{\alpha}.
    It checks whether the coefficient vectors are equivalent.
}
