#
# test <- function(){
#   if (!requireNamespace("dbarts", quietly = TRUE)) {
#     warning("The bart package must be installed")
#     return(NULL)
#   }
#
#   # #read data
#   # tmp_cov0 <- as.matrix(read.csv(file = 'X_0.csv'))
#   tmp_Y0 <-  as.matrix(read.csv(file = 'Y_0.csv'))
#
#   message('Cross-validating BART; should take a minute\r')
#   flush.console()
#   n.trees <- c(100, 200)
#   nu_q <- list(c(3, 0.9), c(3, 0.99), c(10, 0.75))
#   k <- c(2, 3, 5)
#   alpha <- c(0.5, 0.95)[2]
#   beta <- c(.5, 2)[2]
#
#   cv0 <- apply(dbarts::xbart(formula = tmp_cov0,
#                              data = tmp_Y0,
#                              verbose = FALSE,
#                              method = 'k-fold',
#                              n.test = 5,
#                              n.reps = 5,
#                              loss = 'rmse',
#                              n.trees = n.trees,
#                              k = k,
#                              power = beta,
#                              base = alpha),c(2, 3), mean)
#   best_params0 <- arrayInd(which.min(cv0), dim(cv0))
#   n.trees0 <- n.trees[best_params0[1]]
#   k0 <- k[best_params0[2]]
#
# }
# test()
#
#
# set.seed(3221)  # this makes the example exactly reproducible
# my.data <- data.frame(y=rnorm(5),
#                       x1=c(1:5),
#                       x2=c(TRUE, TRUE, FALSE, FALSE, FALSE),
#                       X3=letters[1:5])
# my.data <- as.matrix(my.data)
# sapply(my.data, typeof)
# treated_column_name = 'treated'
# outcome_column_name = 'outcome'
# black_box = "BART"
# user_PE_fit = NULL
# user_PE_fit_params = NULL
# user_PE_predict = NULL
# user_PE_predict_params = NULL
# cv = F
# C = 1.1
#
# missing_data = 'impute'
# missing_holdout = 'impute'
# missing_data_imputations = 'impute'
# missing_holdout_imputations = 'impute'
# impute_with_treatment = TRUE
# impute_with_outcome = FALSE
#
#
# data <- gen_data()
# data[1,"X1"] <- NA
# data[2,"X2"] <- NA
# data["X3"] = as.factor(letters[1:100])
#
# holdout <- data.frame(data)
# # AHB_MIP_match(data = data,holdout =holdout ,missing_dat= 2, missing_holdout = 2)
#
# missing_out <-handle_missing_data(data, holdout,
#                                   treated_column_name, outcome_column_name,
#                                   missing_data, missing_holdout,
#                                   missing_holdout_imputations,
#                                   impute_with_treatment, impute_with_outcome)
