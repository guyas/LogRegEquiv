test_that("performance_equiv works, female test data", {
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

  delta <- 1.1
  eps <- delta^2
  alpha <- 0.05

  d_l <- b_male - (b_female * eps)
  d_u <- b_male - (b_female / eps)

  t_stat_l <- mean(d_l) / sqrt(var(d_l))
  t_stat_u <- mean(d_u) / sqrt(var(d_u))
  threshold <- qt(alpha, df = (n - 1))
  equiv_left <- (threshold < t_stat_l)
  equiv_right <- (t_stat_u < -threshold)
  equivalence <- (equiv_left && equiv_right)

  be_out <- performance_equiv(model_female, model_male,
                                              test_data, dv_index, delta, alpha)
  expect_equal(be_out$brier_score_ac, bs_female)
  expect_equal(be_out$brier_score_bc, bs_male)
  expect_equal(be_out$equivalence, equivalence)
  expect_equal(be_out$test_stat_l, t_stat_l)
  expect_equal(be_out$test_stat_u, t_stat_u)
  expect_equal(be_out$crit_val, threshold)
})


test_that("performance_equiv works, male test data", {
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

  delta <- 1.1
  eps <- delta^2
  alpha <- 0.05

  d_l <- b_male - (b_female * eps)
  d_u <- b_male - (b_female / eps)

  t_stat_l <- mean(d_l) / sqrt(var(d_l))
  t_stat_u <- mean(d_u) / sqrt(var(d_u))
  threshold <- qt(alpha, df = (n - 1), lower.tail = T)
  equiv_left <- (threshold < t_stat_l)
  equiv_right <- (t_stat_u < -threshold)
  equivalence <- (equiv_left && equiv_right)

  be_out <- performance_equiv(model_female, model_male,
                                              test_data, dv_index, delta, alpha)
  expect_equal(be_out$brier_score_ac, bs_female)
  expect_equal(be_out$brier_score_bc, bs_male)
  expect_equal(be_out$equivalence, equivalence)
  expect_equal(be_out$test_stat_l, t_stat_l)
  expect_equal(be_out$test_stat_u, t_stat_u)
  expect_equal(be_out$crit_val, threshold)
})


test_that("performance_equiv works, male test data, delta=1.01", {
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

  delta <- 1.01
  eps <- delta^2
  alpha <- 0.05


  d_l <- b_male - (b_female * eps)
  d_u <- b_male - (b_female / eps)

  t_stat_l <- mean(d_l) / sqrt(var(d_l))
  t_stat_u <- mean(d_u) / sqrt(var(d_u))
  threshold <- qt(alpha, df = (n - 1), lower.tail = T)
  equiv_left <- (threshold < t_stat_l)
  equiv_right <- (t_stat_u < -threshold)
  equivalence <- (equiv_left && equiv_right)

  be_out <- performance_equiv(model_female, model_male,
                                              test_data, dv_index, delta, alpha)
  expect_equal(be_out$brier_score_ac, bs_female)
  expect_equal(be_out$brier_score_bc, bs_male)
  expect_equal(be_out$equivalence, equivalence)
  expect_equal(be_out$diff_sd_l, sqrt(var(d_l)))
  expect_equal(be_out$diff_sd_u, sqrt(var(d_u)))
  expect_equal(be_out$test_stat_l, t_stat_l)
  expect_equal(be_out$test_stat_u, t_stat_u)
  expect_equal(be_out$crit_val, threshold)
})
