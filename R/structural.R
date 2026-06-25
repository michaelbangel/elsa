liability_engine <- function(accuracy, compliance, incidence, remediation){

  score <- (
    accuracy * 0.35 +
    compliance * 0.25 +
    (100 - incidence) * 0.30 +
    remediation * 0.10
  )

  pmin(score, 100)
}

integrity_engine <- function(accuracy, consistency, compliance, incidence){

  score <- (
    accuracy * 0.30 +
    consistency * 0.25 +
    compliance * 0.25 +
    (100 - incidence) * 0.20
  )

  pmin(score, 100)
}

doubt_engine <- function(variance, delay, incidence, deviation){

  score <- (
    variance * 0.25 +
    delay * 0.15 +
    incidence * 0.40 +
    deviation * 0.20
  )

  pmin(score, 100)
}

negotiator_risk <- function(incidence, variance, exception, accuracy){

  score <- (
    incidence * 0.40 +
    variance * 0.25 +
    exception * 0.20 +
    (100 - accuracy) * 0.15
  )

  pmin(score, 100)
}

root_cause <- function(variance, incidence, exception){

  if(variance > 60 && incidence > 60){
    return("Manipulation Risk Pattern")
  }

  if(exception > 70){
    return("System Integrity Failure")
  }

  if(incidence > 50){
    return("Process Control Failure")
  }

  return("Operational Error Pattern")
}
