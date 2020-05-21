test_that("beta_equivalence works with scalar", {
  "ptg_stud_f_train"
  "ptg_stud_m_train"
  model_formula <- "final_fail ~ ."
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = binomial(link = "logit"),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = binomial(link = "logit"),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))

  coef_vec_diff <- model_female$coefficients - model_male$coefficients
  p <- length(coef_vec_diff)
  cov_mat <- summary(model_female)$cov.unscaled +
    summary(model_male)$cov.unscaled
  test_stat <- t(coef_vec_diff) %*% solve(cov_mat) %*% coef_vec_diff
  d_val <- 0.1
  alpha <- 0.05
  d <- rep(d_val, p)
  ncp <-  t(d) %*% solve(cov_mat) %*% d
  crit_value <- qchisq(p = alpha, df = p, ncp = ncp)
  equiv <- (test_stat < crit_value)

  be_out <- beta_equivalence(model_female, model_male, d, alpha)

  expect_equal(be_out$equivalence, equiv)
  expect_equal(be_out$test_statistic, test_stat)
  expect_equal(be_out$critical_value, crit_value)
  expect_equal(be_out$ncp, ncp)

})

test_that("beta_equivalence works with vector", {
  "ptg_stud_f_train"
  "ptg_stud_m_train"
  model_formula <- "final_fail ~ ."
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = binomial(link = "logit"),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = binomial(link = "logit"),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))

  coef_vec_diff <- model_female$coefficients - model_male$coefficients
  p <- length(coef_vec_diff)
  cov_mat <- summary(model_female)$cov.unscaled +
    summary(model_male)$cov.unscaled
  test_stat <- t(coef_vec_diff) %*% solve(cov_mat) %*% coef_vec_diff
  d_val <- 0.1
  alpha <- 0.05
  d <- runif(p, 0, 0.25)
  ncp <-  t(d) %*% solve(cov_mat) %*% d
  crit_value <- qchisq(p = alpha, df = p, ncp = ncp)
  equiv <- (test_stat < crit_value)

  be_out <- beta_equivalence(model_female, model_male, d, alpha)

  expect_equal(be_out$equivalence, equiv)
  expect_equal(be_out$test_statistic, test_stat)
  expect_equal(be_out$critical_value, crit_value)
  expect_equal(be_out$ncp, ncp)

})

test_that("beta_equivalence rejects mismatching families #1", {
  "ptg_stud_f_train"
  "ptg_stud_m_train"
  model_formula <- "final_fail ~ ."
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = binomial(link = "logit"),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = gaussian(),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))
  expect_error(beta_equivalence(model_female, model_male, 0.1, 0.05),
               "Mismatching model families")
})


test_that("beta_equivalence rejects mismatching families #2", {
  "ptg_stud_f_train"
  "ptg_stud_m_train"
  model_formula <- "final_fail ~ ."
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = gaussian(),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = binomial(link = "logit"),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))
  expect_error(beta_equivalence(model_female, model_male, 0.1, 0.05),
               "Mismatching model families")
})


test_that("beta_equivalence rejects mismatching families #3", {
  "ptg_stud_f_train"
  "ptg_stud_m_train"
  model_formula <- "final_fail ~ ."
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = gaussian(),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = gaussian(),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))
  expect_error(beta_equivalence(model_female, model_male, 0.1, 0.05),
               "Mismatching model families")
})

test_that("beta_equivalence rejects mismatching formulae", {
  "ptg_stud_f_train"
  "ptg_stud_m_train"
  model_female <- suppressWarnings(glm(formula = final_fail ~ .,
                                       family = binomial(link = "logit"),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = final_fail ~ . - 1,
                    family = binomial(link = "logit"),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))
  expect_error(beta_equivalence(model_female, model_male, 0.1, 0.05),
               "Mismatching regression formulae")
})

test_that("beta_equivalence rejects mismatching covariates #1", {
  "ptg_stud_f_train"
  "ptg_stud_m_train"
  model_formula <- "final_fail ~ ."
  colnames(ptg_stud_f_train)[1] <- "escola"
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = binomial(link = "logit"),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = binomial(link = "logit"),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))
  expect_error(beta_equivalence(model_female, model_male, 0.1, 0.05),
               "Mismatching covariates")
})

test_that("beta_equivalence rejects mismatching covariates #2", {
  "ptg_stud_f_train"
  "ptg_stud_m_train"
  model_formula <- "final_fail ~ ."
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = binomial(link = "logit"),
                                       data = ptg_stud_f_train[, -4],
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = binomial(link = "logit"),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))
  expect_error(beta_equivalence(model_female, model_male, 0.1, 0.05),
               "Mismatching covariates")
})


test_that("beta_equivalence rejects mismatching delta vector", {
  "ptg_stud_f_train"
  "ptg_stud_m_train"
  model_formula <- "final_fail ~ ."
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = binomial(link = "logit"),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = binomial(link = "logit"),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))
  expect_error(beta_equivalence(model_female, model_male, runif(10), 0.05),
               "Mismatching delta vector")
})
