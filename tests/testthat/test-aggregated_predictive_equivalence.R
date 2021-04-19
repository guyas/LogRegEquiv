test_that("aggregated_predictive_equivalence works, female test data", {
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
  dv_index <- 30
  y_test <- test_data[, dv_index]
  pi_hat_female <- predict.glm(model_female, test_data, type = "response")
  b_female <- (y_test - pi_hat_female)^2
  bs_female <- mean(b_female)

  pi_hat_male <- predict.glm(model_male, test_data, type = "response")
  b_male <- (y_test - pi_hat_male)^2
  bs_male <- mean(b_male)

  d <- b_female - b_male

  t <- 0.1
  delta <- mean(abs(y_test - pi_hat_female)) * 2 * t - t^2
  alpha <- 0.05

  t_stat_l <- sqrt(n) * (mean(d) + delta) / sqrt(var(d))
  t_stat_u <- sqrt(n) * (mean(d) - delta) / sqrt(var(d))
  threshold <- qt(1 - alpha, df = (n - 1), lower.tail = T)
  equiv_left <- (threshold < t_stat_l)
  equiv_right <- (t_stat_u < -threshold)
  equivalence <- (equiv_left && equiv_right)

  be_out <- aggregated_predictive_equivalence(model_female, model_male, test_data, dv_index,
                                              alpha, t)
  expect_equal(be_out$brier_score_ac, bs_female)
  expect_equal(be_out$brier_score_bc, bs_male)
  expect_equal(be_out$equivalence, equivalence)
  expect_equal(be_out$test_stat_l, t_stat_l)
  expect_equal(be_out$test_stat_u, t_stat_u)
  expect_equal(be_out$crit_val, threshold)
})


test_that("aggregated_predictive_equivalence works, male test data", {
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
  dv_index <- 30
  y_test <- test_data[, dv_index]
  pi_hat_female <- predict.glm(model_female, test_data, type = "response")
  b_female <- (y_test - pi_hat_female)^2
  bs_female <- mean(b_female)

  pi_hat_male <- predict.glm(model_male, test_data, type = "response")
  b_male <- (y_test - pi_hat_male)^2
  bs_male <- mean(b_male)

  d <- b_female - b_male

  t <- 0.1
  delta <- mean(abs(y_test - pi_hat_female)) * 2 * t - t^2
  alpha <- 0.05

  t_stat_l <- sqrt(n) * (mean(d) + delta) / sqrt(var(d))
  t_stat_u <- sqrt(n) * (mean(d) - delta) / sqrt(var(d))
  threshold <- qt(1 - alpha, df = (n - 1), lower.tail = T)
  equiv_left <- (threshold < t_stat_l)
  equiv_right <- (t_stat_u < -threshold)
  equivalence <- (equiv_left && equiv_right)

  be_out <- aggregated_predictive_equivalence(model_female, model_male, test_data, dv_index,
                                              alpha, t)
  expect_equal(be_out$brier_score_ac, bs_female)
  expect_equal(be_out$brier_score_bc, bs_male)
  expect_equal(be_out$equivalence, equivalence)
  expect_equal(be_out$test_stat_l, t_stat_l)
  expect_equal(be_out$test_stat_u, t_stat_u)
  expect_equal(be_out$crit_val, threshold)
})


test_that("aggregated_predictive_equivalence works, male test data, t=0.01", {
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
  dv_index <- 30
  y_test <- test_data[, dv_index]
  pi_hat_female <- predict.glm(model_female, test_data, type = "response")
  b_female <- (y_test - pi_hat_female)^2
  bs_female <- mean(b_female)

  pi_hat_male <- predict.glm(model_male, test_data, type = "response")
  b_male <- (y_test - pi_hat_male)^2
  bs_male <- mean(b_male)

  d <- b_female - b_male

  t <- 0.01
  delta <- mean(abs(y_test - pi_hat_female)) * 2 * t - t^2
  alpha <- 0.05

  t_stat_l <- sqrt(n) * (mean(d) + delta) / sqrt(var(d))
  t_stat_u <- sqrt(n) * (mean(d) - delta) / sqrt(var(d))
  threshold <- qt(1 - alpha, df = (n - 1), lower.tail = T)
  equiv_left <- (threshold < t_stat_l)
  equiv_right <- (t_stat_u < -threshold)
  equivalence <- (equiv_left && equiv_right)

  be_out <- aggregated_predictive_equivalence(model_female, model_male, test_data, dv_index,
                                              alpha, t)
  expect_equal(be_out$brier_score_ac, bs_female)
  expect_equal(be_out$brier_score_bc, bs_male)
  expect_equal(be_out$equivalence, equivalence)
  expect_equal(be_out$diff_sd, sqrt(var(d)))
  expect_equal(be_out$test_stat_l, t_stat_l)
  expect_equal(be_out$test_stat_u, t_stat_u)
  expect_equal(be_out$crit_val, threshold)
})
