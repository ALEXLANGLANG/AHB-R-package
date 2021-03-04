test_that("read_data ", {
  invalidFile = "123.csv"
  validHoldoutFile = "./holdout.csv"
  vlidDataFile = "./data.csv"
  df <- gen_data(n_units = 100, p = 5)
  holdout <- gen_data(n_units = 100, p = 5)
  expect_error(read_data(invalidFile, df,"treated", "outcome"))
  expect_error(read_data(df, invalidFile,"treated", "outcome"))
  list_df <- read_data(df, holdout,"treated", "outcome")
  expect_equal(list_df[[1]],df)
  expect_equal(list_df[[2]],holdout)
  read_data(vlidDataFile,validHoldoutFile,"treated", "outcome")
})
