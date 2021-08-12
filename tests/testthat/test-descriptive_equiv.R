test_that("descriptive_equivalence works", {
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
  pval <- pchisq(q = test_stat, df = p, ncp = ncp)

  cve_out <- suppressWarnings(descriptive_equiv(data_a = ptg_stud_f_train,
                                     data_b = ptg_stud_m_train,
                                     formula = model_formula,
                                     delta =  d,
                                     alpha =  alpha))

  expect_equal(cve_out$equivalence$equivalence, equiv)
  expect_equal(cve_out$equivalence$test_statistic, test_stat)
  expect_equal(cve_out$equivalence$critical_value, crit_value)
  expect_equal(cve_out$equivalence$ncp, ncp)
  expect_equal(cve_out$equivalence$p_value, pval)
})
