#start to work on your functions and analysis
# Oct 2025
# Purpose: Lecture 5
# Information criteria

# InfCriteriaV3 (loglikelihood = -5080,
#               clusters = 2,
#               dimensions = 3,
#               nObservations = 1000)

InfCriteriaV3 <- function(loglikelihood, clusters,
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
