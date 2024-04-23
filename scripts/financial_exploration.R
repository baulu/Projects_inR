
library(tidyverse)

# Example usage:
own_capital <- 50000  # Eigenkapital
total_debt <- 200000  # Fremdkapital
interest_rate <- 0.01  # Annual interest rate
amortization_rate <- 0.05  # Annual amortization rate
years <- 20  # Number of years for projection

financial_projection <- project_financials(own_capital, total_debt, interest_rate, amortization_rate, years) 
financial_projection <- financial_projection %>% 
  mutate(Fremdverschuldung = Fremdkapital / (Eigenkapital + Fremdkapital))
print(financial_projection)

  
