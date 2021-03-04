# author: Xian Sun, Duke University, 2020

read_data <- function(data, holdout,
                      treated_column_name, outcome_column_name) {
  if (is.character(data)) {
      data <- read.csv(data, header = TRUE)
  }

  if (is.character(holdout)) {
      holdout <- read.csv(holdout, header = TRUE)
  }

  if (is.numeric(holdout) & length(holdout) == 1) {
    holdout_inds <- sample(1:nrow(data), size = round(holdout * nrow(data)))
    holdout <- data[holdout_inds, ]
    data <- data[-holdout_inds, ]
    rownames(data)<-NULL
    rownames(holdout)<-NULL
  }

  return(list(data = data,
              holdout = holdout))
}
