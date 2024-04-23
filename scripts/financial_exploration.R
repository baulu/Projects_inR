
library(tidyverse)

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

# Example usage:
own_capital <- 50000  # Eigenkapital
total_debt <- 200000  # Fremdkapital
interest_rate <- 0.05  # Annual interest rate
amortization_rate <- 0.02  # Annual amortization rate
years <- 20  # Number of years for projection

financial_projection <- project_financials(own_capital, total_debt, interest_rate, amortization_rate, years) 
financial_projection <- financial_projection %>% 
  mutate(Fremdverschuldung = Fremdkapital / (Eigenkapital + Fremdkapital))
print(financial_projection)

ggplot(financial_projection)+
  geom_line(aes(x = Year, y = Fremdkapital), color = "red") +
  theme_classic() 

ggplot(financial_projection)+
  geom_line(aes(x = Year, y = Fremdverschuldung), color = "pink") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_classic() 
  
