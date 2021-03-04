# train_df <- gen_mixedData()
# test_df <- gen_mixedData()
# treated_column_name = 'treated'
# outcome_column_name = 'outcome'
# user_PE_fit = NULL
# user_PE_fit_params = NULL
# user_PE_predict = NULL
# user_PE_predict_params = NULL
# black_box='xgb'
# cv= T
# n_train <- nrow(train_df)
# n_units <- nrow(train_df) + nrow(test_df)
# p <- ncol(train_df) - 2
# #get indexes
# train_treat_col_ind <- which(colnames(train_df) == treated_column_name)
# train_out_col_ind <- which(colnames(train_df) == outcome_column_name)
# test_treat_col_ind <- which(colnames(test_df) == treated_column_name)
# test_out_col_ind <- which(colnames(test_df) == outcome_column_name)
# test_covs_ind <- which(colnames(test_df) != treated_column_name & colnames(test_df) != outcome_column_name)
# train_covs_ind <- which(colnames(train_df) != treated_column_name & colnames(train_df) != outcome_column_name)
# #Get data for later use
# train_df[,train_treat_col_ind] <- train_df[,train_treat_col_ind] == 1
# test_df[,test_treat_col_ind] <- test_df[,test_treat_col_ind] == 1
# train_covs <- as.matrix(train_df[, train_covs_ind])
# train_control <- which(!train_df[,train_treat_col_ind] )
# train_treated <- which(train_df[,train_treat_col_ind])
# test_covs <- as.matrix(test_df[, test_covs_ind])
# test_control <- which(!test_df[,test_treat_col_ind])
# test_treated <- which(test_df[,test_treat_col_ind])
# n_test_control <- length(test_control)
# n_test_treated <- length(test_treated)
# tmp_cov1 <- as.matrix(train_df[which(train_df[,train_treat_col_ind]), train_covs_ind])
# tmp_Y1 <-  as.matrix(train_df[which(train_df[,train_treat_col_ind]), train_out_col_ind])
# tmp_cov0 <-  as.matrix(train_df[which(!train_df[,train_treat_col_ind]), train_covs_ind])
# tmp_Y0 <-  as.matrix(train_df[which(!train_df[,train_treat_col_ind]), train_out_col_ind])
#
# if (!is.null(user_PE_fit)) {
#   PE_fit <- user_PE_fit
#   PE_fit_params <- user_PE_fit_params
# }
#
# PE_fit <- dbarts::xbart
# PE_fit_params <- list(keeptrees = TRUE,
#                       keepevery = 10,
#                       verbose = FALSE,
#                       k = 2,
#                       n.trees=200)
#
# bbf0 <- do.call(PE_fit, c(list(tmp_cov0, tmp_Y0), PE_fit_params))
# bbf1 <- do.call(PE_fit, c(list(tmp_cov1, tmp_Y1), PE_fit_params))
#
