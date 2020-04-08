setRegressors <- function(dates, interventions.past, interventions.future){
  
  past <- rep(0, length(dates))
  keep <- which(dates %in% interventions.past)
  past[keep] <- 1
  
  
  future <- rep(0, 10)
  keep <- which(dates %in% interventions.future)
  future[keep] <- 1
  
  regg <- list()
  regg$reggt <- past
  regg$reggp <- future
  
  return(regg)
}