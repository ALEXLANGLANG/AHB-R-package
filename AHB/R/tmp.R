# gen_missing <- function(){
#   data <- gen_data()
#   data[1,"X1"] <- NA
#   data[2,"X2"] <- NA
#   data["X3"] = as.factor(letters[1:100])
#   data["X4"] <- "a"
#   data[c(1,2,3,4),"X4"]<-"b"
#   data["X4"] <-
#     lapply(data[, "X4", drop = FALSE], as.factor)
#   return (data)
# }
#
# data <- gen_missing()
# data <- mapCategoricalToFactor(data,treated_column_name = 'treated',outcome_column_name = 'outcome')
# data<-handle_missing(data,"data",missing_data = "drop",'treated','outcome', T, F)
# data_dummy<-mapFactorToDummy(data, treated_column_name, outcome_column_name)
# model.matrix(~., data=data)[,-1]
