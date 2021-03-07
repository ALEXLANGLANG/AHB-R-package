#' ATE of a matched dataset
#'
#' \code{ATE} computes the average treatment effect (ATE) of a matched dataset.
#'
#' The ATE is computed as the difference between the weighted treated and the
#' weighted control outcomes in the dataset. A unit's weight is the number of
#' times it was matched.
#'
#' @param AHB_out An object returned by running \code{\link{AHB_fast_match}} and
#'   \code{\link{AHB_MIP_match}}
#' @export

ATE <- function(AHB_out){
  CATEs <- AHB_out$CATE
  MGs <- AHB_out$MGs
  weight <- vector('numeric', length = nrow(AHB_out$data))
  for(i in 1:length(MGs)){
    weight[MGs[[i]]] <- weight[MGs[[i]]] + 1
  }

  weight_sum <- 0
  weighted_CATE_sum <- 0

  for (j in 1:length(MGs)) {
    MG_weight <- sum(weight[MGs[[j]]])
    weight_sum <- weight_sum + MG_weight
    weighted_CATE_sum <- weighted_CATE_sum + MG_weight * CATEs[[j]]
  }
  ATE <- weighted_CATE_sum / weight_sum

  return (ATE)
}

#' ATT of a matched dataset
#'
#' \code{ATT} computes the average treatment effect on the treated (ATT) of a
#' matched dataset.
#'
#' The counterfactual outcome of each treated unit is estimated via the mean
#' outcome of control units in its matched group. This value is then averaged
#' across all treated units to compute the ATT.
#' @param AHB_out An object returned by running \code{\link{AHB_fast_match}} and
#'   \code{\link{AHB_MIP_match}}
#' @export
ATT <- function(AHB_out) {
  ind_treated <- which(colnames(AHB_out$data) == AHB_out$verbose[1])
  ind_outcome <- which(colnames(AHB_out$data) == AHB_out$verbose[2])
  controls <-  which(AHB_out$data[, ind_treated] == 0)
  treated <- which(AHB_out$data[, ind_treated] == 1)
  outcomes <- AHB_out$data[,ind_outcome]
  MGs <- AHB_out$MGs

  weight <- vector('numeric', length = nrow(AHB_out$data))
  for(i in 1:length(MGs)){
    weight[MGs[[i]]] <- weight[MGs[[i]]] + 1
  }

  weight_sum <- 0
  weighted_TT_sum <- 0
  for (j in 1:length(MGs)) {
    MG_controls <- MGs[[j]][MGs[[j]] %in% controls]
    MG_treated <- MGs[[j]][MGs[[j]] %in% treated]

    MG_weight <- sum(weight[MG_controls])
    weight_sum <- weight_sum + MG_weight
    mean_control_outcome <- mean(outcomes[MG_controls])

    for (k in seq_along(MG_treated)) {
      weighted_TT_sum <-
        weighted_TT_sum +
        MG_weight * (outcomes[MG_treated[k]] - mean_control_outcome)/length(MG_treated)
    }
  }

  ATT <- weighted_TT_sum / weight_sum
  return(ATT)
}



print_box<-function(AHB_out, treated_unit_id){

}
