variance_predictor <- function(variance, trend, incidence, consistency){

  score <- (
    variance * 0.30 +
    trend * 0.20 +
    incidence * 0.30 +
    (100 - consistency) * 0.20
  )

  pmin(score, 100)
}

variance_acceleration <- function(v1, v2, v3){
  abs((v3 - v2) + (v2 - v1))
}

incidence_momentum <- function(i1, i2){
  abs(i2 - i1)
}
