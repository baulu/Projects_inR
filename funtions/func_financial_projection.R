project_financials <- function(own_capital, total_debt, interest_rate, amortization_rate, years) {
  equity <- numeric(length = years)
  debt <- numeric(length = years)
  mortgage_payments <- numeric(length = years)
  amortization_payments <- numeric(length = years)
  
  equity[1] <- own_capital
  debt[1] <- total_debt
  amortization_payments[1] <- total_debt*amortization_rate
  
  for (i in 2:years) {
    mortgage_payments[i] <- debt[i - 1] * interest_rate
    amortization_payments[i] <- debt[i - 1] * amortization_rate
    equity[i] <- equity[i - 1]
    debt[i] <- debt[i - 1] - mortgage_payments[i] - amortization_payments[i]
  }
  
  result <- data.frame(
    Year = 1:years,
    Eigenkapital = equity,
    Fremdkapital = debt,
    Jaehrliche_Zinszahlung = mortgage_payments,
    Jaehrliche_Amortisationszahlung = amortization_payments
  )
  
  return(result)
}