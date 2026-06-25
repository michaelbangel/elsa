broken_settlement_engine <- function(settlement_closed, reopened, dispute, legal_reversal){

  if(settlement_closed &&
     (reopened || dispute || legal_reversal)){
    return(100)
  }

  return(0)
}

post_settlement_income_variance <- function(reported, verified, broken){

  if(!broken) return(0)
  if(verified <= 0) return(0)

  abs((reported - verified) / verified) * 100
}

post_settlement_expense_variance <- function(reported, verified, broken){

  if(!broken) return(0)
  if(verified <= 0) return(0)

  abs((reported - verified) / verified) * 100
}

broken_settlement_trigger <- function(broken, income_var, expense_var, variance, incidence){

  if(broken &&
     (income_var > 10 || expense_var > 10 || variance > 10 || incidence > 30)){
    return(1.5)
  }

  if(broken){
    return(1.25)
  }

  return(1.0)
}
