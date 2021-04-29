test_that("individual_predictive_equivalence works, female test data", {
  "ptg_stud_f_train"
  "ptg_stud_f_test"
  "ptg_stud_m_train"
  "ptg_stud_m_test"
  model_formula <- "final_fail ~ ."
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = binomial(link = "logit"),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = binomial(link = "logit"),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))
  test_data <- ptg_stud_f_test
  n <- nrow(test_data)
  r <- 0.05
  alpha <- 0.05
  theta_female <- predict.glm(model_female, test_data)
  theta_male <- predict.glm(model_male, test_data)
  delta <- as.numeric(quantile(abs(theta_female), r))
  xi <- abs(theta_female - theta_male)
  t_stat <- (mean(xi) - delta) / (sd(xi) / sqrt(n))
  crit_value <- qt(alpha, df = n - 1, lower.tail = FALSE)
  equiv <- (t_stat < crit_value)
  te_out <- individual_predictive_equivalence(model_female, model_male,
                                              test_data, r, alpha)
  expect_equal(te_out$equivalence, equiv)
  expect_equal(te_out$test_statistic, t_stat)
  expect_equal(te_out$critical_value, crit_value)
  expect_equal(te_out$xi_bar, mean(xi))
  expect_equal(te_out$delta_theta, delta)
})

test_that("individual_predictive_equivalence works, male test data", {
  "ptg_stud_f_train"
  "ptg_stud_f_test"
  "ptg_stud_m_train"
  "ptg_stud_m_test"
  model_formula <- "final_fail ~ ."
  model_female <- suppressWarnings(glm(formula = as.formula(model_formula),
                                       family = binomial(link = "logit"),
                                       data = ptg_stud_f_train,
                                       control = list(maxit = 20)))
  model_male <- glm(formula = as.formula(model_formula),
                    family = binomial(link = "logit"),
                    data = ptg_stud_m_train,
                    control = list(maxit = 20))
  test_data <- ptg_stud_m_test
  n <- nrow(test_data)
  r <- 0.1
  alpha <- 0.01
  theta_female <- predict.glm(model_female, test_data)
  theta_male <- predict.glm(model_male, test_data)
  delta <- as.numeric(quantile(abs(theta_male), r))
  xi <- abs(theta_female - theta_male)
  t_stat <- (mean(xi) - delta) / (sd(xi) / sqrt(n))
  crit_value <- qt(alpha, df = n - 1, lower.tail = FALSE)
  equiv <- (t_stat < crit_value)
  te_out <- individual_predictive_equivalence(model_male, model_female,
                                              test_data, r, alpha)
  expect_equal(te_out$equivalence, equiv)
  expect_equal(te_out$test_statistic, t_stat)
  expect_equal(te_out$critical_value, crit_value)
  expect_equal(te_out$xi_bar, mean(xi))
  expect_equal(te_out$delta_theta, delta)
})
