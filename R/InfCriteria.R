#' Calculate Information Criteria Value for Model Selection
#'
#' Calculates information criteria values, BIC and AIC for model
#' selection in Gaussian mixture models, provided loglikelihood
#' number of clusters, dimensionality of dataset, and number
#' of observations
#'
#' @examples
#' InfCriteria (loglikelihood = -5080,
#'                clusters = 2,
#'                dimensions = 3,
#'               nObservations = 1000)
#'
#'
#' @export


InfCriteria <- function(loglikelihood, clusters,
                          dimensions, nObservations = 100) {

  # calculate number of parameters based on Gaussian mixture
  kParameters <- ((dimensions + 1) * dimensions) / 2 +
    dimensions + (clusters - 1)

  # calculate model selection criteria
  BIC <- -2 * loglikelihood + (kParameters * log(nObservations))
  AIC <- -2 * loglikelihood + (2 * kParameters)

  Results <- list(BICresults = BIC,
                  AICresults = AIC)

  class(Results) <- "InfCriteria"
  return(Results)
}

# InfCriteriaV3 (loglikelihood = -5080,
#                clusters = 2,
#                dimensions = 3,
#               nObservations = 1000)
