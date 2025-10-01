# Oct 2025
# Purpose: Lecture 5
# Information criteria

# InfCriteriaPlot(infValues = c(10229.08, 10180))

InfCriteriaPlotV3 <- function(infValues,
                              plotTitle = "Information Criteria Value Plot") {
  # plotting the information criteria values
  plotExample <- graphics::plot(unlist(infValues), type = "p",
                                ylab = "Information Criteria Value",
                                xlab = "Information Criterion",
                                xaxt = "n",
                                main = "Example Plot")
  plotAxis <- plotExample + axis(1, at = 1:2,
                                 labels = c("BIC", "AIC"))
  return(plotAxis)
}

#InfCriteriaPlot(infValues = c(10229.08, 10180))

