#
# for(i in 1:5){
#   p <- 5
#   df <- gen_data(n_units = 100, p = p)
#   MIP_out <- AHB_MIP_match(data = df, holdout = 0.5,black_box='BART',  missing_data = "none", missing_holdout = 0,
#                            missing_data_imputations = 5, missing_holdout_imputations = 5,cv=T,
#                            impute_with_treatment = TRUE, impute_with_outcome = TRUE)
# }

#
# #
# if (!requireNamespace("glmnet", quietly = TRUE)) {
#   warning("The glmnet package must be installed")
#   return(NULL)
# }
#
# if (!requireNamespace("xgboost", quietly = TRUE)) {
#   warning("The xgboost package must be installed")
#   return(NULL)
# }
# if (!requireNamespace("dbarts", quietly = TRUE)) {
#   warning("The bart package must be installed")
#   return(NULL)
# }
#
# PE_fit <- glmnet::glmnet
# PE_fit_params <- list(alpha = 1, lambda = 0.2)





# PE_fit <- dbarts::bart
# PE_fit_params <-   list(keeptrees = TRUE,
#                         keepevery = 10,
#                         verbose = FALSE,
#                         k = 3,
#                         ntree = 3)

# PE_fit <- xgboost::xgboost
# best_params0 <- list(eta = 1, max_depth = 2, alpha = 0.5, subsample = 0.5, nrounds = 2)
# PE_fit_params <- list(params = list(objective = 'reg:squarederror',
#               eta = best_params0$eta,
#               max_depth = best_params0$max_depth,
#               alpha = best_params0$alpha,
#               subsample = best_params0$subsample),
#               nrounds = best_params0$nrounds,
#               verbose = 0)
#
# user_PE_fit <- PE_fit
# user_PE_fit_params <- PE_fit_params
# user_PE_predict <- predict
# user_PE_predict_params <- NULL
# p <- 5
# df <- gen_data(n_units = 100, p = p)
# fast_out <- AHB_fast_match(data = df, holdout = 0.5,n_prune = 50,user_PE_fit =user_PE_fit,
#                            user_PE_fit_params=user_PE_fit_params,user_PE_predict = user_PE_predict,
#                            user_PE_predict_params = user_PE_predict_params
#                            )
# fast_out <- AHB_MIP_match(data = df, holdout = 0.5,n_prune = 50, black_box = "xgb")
#
# #
# # cv_output1 <- glmnet::cv.glmnet(tmp_cov1, tmp_Y1,
# #                                 alpha = 1, lambda = lambda_seq,
# #                                 nfolds = 5)
# train_df <- gen_data()
# test_df <- gen_data()
# outcome_column_name = 'outcome'
# treated_column_name = 'treated'
# n_train <- nrow(train_df)
# n_units <- nrow(train_df) + nrow(test_df)
# p <- ncol(train_df) - 2
# #f <- formula(paste('treated ~', paste(colnames(train_df)[1:p], collapse = ' + ')))
#
# train_treat_col_ind <- which(colnames(train_df) == treated_column_name)
# train_out_col_ind <- which(colnames(train_df) == outcome_column_name)
# test_treat_col_ind <- which(colnames(test_df) == treated_column_name)
# test_out_col_ind <- which(colnames(test_df) == outcome_column_name)
# test_covs_ind <- which(colnames(test_df) != treated_column_name & colnames(test_df) != outcome_column_name)
# train_covs_ind <- which(colnames(train_df) != treated_column_name & colnames(train_df) != outcome_column_name)
#
# train_df[,train_treat_col_ind] <- train_df[,train_treat_col_ind] == 1
# test_df[,test_treat_col_ind] <- test_df[,test_treat_col_ind] == 1
#
# train_control <- which(!train_df[,train_treat_col_ind] )
# train_treated <- which(train_df[,train_treat_col_ind])
#
# # test_covs <- test_df[, 1:p]
# test_covs <- test_df[, test_covs_ind]
#
# test_control <- which(!test_df[,test_treat_col_ind])
# test_treated <- which(test_df[,test_treat_col_ind])
#
#
# train_covs <- train_df[, train_covs_ind]
#
# n_test_control <- length(test_control)
# n_test_treated <- length(test_treated)
#
# train_df1 <- train_df[which(train_df[,train_treat_col_ind]==T),which(colnames(train_df) != treated_column_name) ]
# train_df0 <- train_df[which(train_df[,train_treat_col_ind]==F),which(colnames(train_df) != treated_column_name) ]
#
# tmp_cov1 <- as.matrix(train_df1[, which(colnames(train_df1) != outcome_column_name)])
# tmp_Y1 <-  as.matrix(train_df1[, which(colnames(train_df1) == outcome_column_name)])
# tmp_cov0 <-  as.matrix(train_df0[, which(colnames(train_df0) != outcome_column_name)])
# tmp_Y0 <-  as.matrix(train_df0[, which(colnames(train_df0) == outcome_column_name)])
#
# X <- tmp_cov1
# Y <- tmp_Y1
#
# PE_fit_params<- NULL #list(alpha = 1, lambda = 0.5)


# fit <- do.call(PE_fit, c(list(X, Y), PE_fit_params))
# PE_predict_params <- NULL
# PE_predict = predict
# preds <- do.call(PE_predict, c(list(fit, X), PE_predict_params))
# assign("PE_predict_params", PE_predict_params, envir = .GlobalEnv)
#
# PE_predict <- function(bart_fit, test_covs, PE_predict_params){
#   a <- do.call(predict, c(list(bart_fit, as.matrix(test_covs)), PE_predict_params))
#   preds<-colMeans(a)
# }
#
# a <- do.call(predict, c(list(fit, as.matrix(X)), PE_predict_params))
#
# fhat1 <- do.call(PE_predict,c(fit, X, PE_predict_params))
#
#
# if (!is.null(user_PE_fit)) {
#   PE_fit <- user_PE_fit
#   PE_fit_params <- user_PE_fit_params
#
#   user_backpassed <- TRUE
# }
#
#
# if (!is.null(user_PE_predict)) {
#   user_backpassed <- TRUE
#   PE_predict <- user_PE_predict
#   PE_predict_params <- user_PE_predict_params
# }
#
# if (user_backpassed) {
#   user_fit_predict <- function(X, Y) {
#     fit <- do.call(PE_fit, c(list(X, Y), PE_fit_params))
#     preds <- do.call(PE_predict, c(list(fit, X), PE_predict_params))
#   }
# }
#
# do_prediction <- function(test_covs){
#   preds <- do.call(PE_predict, c(list(bart_fit1, as.matrix(test_covs)), PE_predict_params))
# }
# do_prediction(X)
