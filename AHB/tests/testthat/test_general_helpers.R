
check_estimator_inputs_helper <- function(user_PE_fit = NULL, user_PE_fit_params = NULL,
                                          user_PE_predict = NULL, user_PE_predict_params = NULL,
                                          black_box='xgb', cv= T){
  train_df <- gen_mixedData()
  test_df <- gen_mixedData()
  inputs <- estimator_inputs(train_df, test_df,
                             "treated","outcome",
                             user_PE_fit, user_PE_fit_params,
                             user_PE_predict, user_PE_predict_params,
                             black_box, cv)
  n_train = inputs[[3]]
  p = inputs[[4]]
  test_df1 = inputs[[9]]
  test_covs = inputs[[10]]
  test_control = inputs[[11]]

  test_treated = inputs[[12]]
  n_test_control = inputs[[13]]
  n_test_treated = inputs[[14]]
  bart_fit0 = inputs[[15]]
  bart_fit1 = inputs[[16]]
  expect_equal(TRUE, !is.null(bart_fit0))
  expect_equal(TRUE, !is.null(bart_fit1))
  expect_equal(TRUE,NCOL(test_covs)==(NCOL(test_df) -2))
  expect_equal(NROW(test_df1), NROW(test_df))
}

test_that("estimator_inputs", {
  check_estimator_inputs_helper()
  check_estimator_inputs_helper(black_box='xgb', cv= F)
  check_estimator_inputs_helper(black_box='xgb', cv= T)
  check_estimator_inputs_helper(black_box='BART', cv= T)
  check_estimator_inputs_helper(black_box='BART', cv= F)
})
