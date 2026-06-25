fii_engine <- function(variance, liability, doubt, negotiator, settlement){

  score <- (
    variance * 0.25 +
    (100 - liability) * 0.20 +
    doubt * 0.15 +
    negotiator * 0.20 +
    settlement * 0.20
  )

  pmin(score, 100)
}

priority_engine <- function(fraud, fii, legal){

  if(legal && (fraud >= 80 || fii >= 80)) return("Critical Priority")
  if(legal && (fraud >= 60 || fii >= 60)) return("High Priority")
  if(legal) return("Elevated")
  return("Standard")
}
