variance_engine <- function(reported, verified){
  if(verified <= 0) return(0)
  abs((reported - verified) / verified) * 100
}

incidence_engine <- function(incorrect, total){
  if(total <= 0) return(0)
  pmin((incorrect / total) * 100, 100)
}

consistency_engine <- function(successful, total){
  if(total <= 0) return(100)
  pmin((successful / total) * 100, 100)
}

trend_engine <- function(current, previous){
  pmin(abs(current - previous), 100)
}

trigger_4pct <- function(variance){
  ifelse(variance >= 4, 100, 0)
}
