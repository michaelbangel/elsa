variance_engine <- function(reported, verified){
  if(verified <= 0) return(0)
  abs((reported - verified) / verified) * 100
}
