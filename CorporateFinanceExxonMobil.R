#Corporate Reporting Discounted Cash flow model Valuation
##Core Figures for estimates from 2018 start year in millions
##Functions for model
####growth functions####
linear_growth_function <- function(start, rate, n) {
  growth_rates <- rep(0, n)
  for (i in seq_len(n)) {
    growth_rates[i] <- start * (1 + rate) ^ i
  }
  return(growth_rates)
}

####Single Year Functions####
###Will calculate single year revenue based on oil price and barrels sold
revenue_prediction <- function(oil, barrel) {
  revenue_predicted <- oil * barrel
}
###Will calculate single year cashflow
future_cashflow_forecast <- function(revenue, operating_margin, tax_rate,reinvestment_rate) 
  { operating_profit <- revenue * operating_margin
    tax_deducted <- operating_profit * tax_rate
    reinvestment <- operating_profit * reinvestment_rate
    future_cashflow <- operating_profit - tax_deducted - reinvestment
    return(future_cashflow)
   }

###Will calculate single year PV where n is year
PV_calculation <- function(future_cashflow, cost_of_capital, n) 
  { cumulative_discount_factor <-  (1 / (1 + cost_of_capital)) ^ n
    present_value <- future_cashflow * cumulative_discount_factor
    return(present_value)
   }

###Will calculate single year PV where future_cashflow_last is terminal year
terminal_value_calculation <- function(future_cashflow_last, cost_of_capital,
                                       terminal_growth_rate) 
  { value <- future_cashflow_last / (cost_of_capital - terminal_growth_rate)
    return(value)
  }

####End of single Year Functions####

####Forecast Functions####
###Will calculate the total_present_value less terminal year where n is terminal year
###oil_price and barrels_sold must be numeric vectors of equal length n.
###Chemical Revenue and Downstream revenue should be vectors of length n
###Vector of constants should be in order operating_margin, tax_rate, reinvestment_rate, cost_of_capital
total_PV_forecast <- function(oil_price, barrels_sold, chemical_revenue, downstream_revenue, n, Modelconstants)
  { PV_vector <- rep(0, (n - 1))
    for (i in seq_len(n - 1)) {
      upstream_revenue <- revenue_prediction(oil_price[i], barrels_sold[i])
      revenue <- upstream_revenue + chemical_revenue[i] + downstream_revenue[i]
      FCFF <- future_cashflow_forecast(revenue,
                                 Modelconstants[1],
                                 Modelconstants[2],
                                 Modelconstants[3])
      PV_value <- PV_calculation(FCFF, Modelconstants[4], i)
      #print(paste0("The PV of FCFF for year ", i, " ", PV_value))
      PV_vector[i] <- PV_value
                                }
    return(sum(PV_vector))
  }

PV_forecast_table <-
  function(oil_price, barrels_sold, chemical_revenue, downstream_revenue, n, Modelconstants)
    { PV_vector <- rep(0, (n - 1))
      years <- rep(0, (n - 1))
      revenue_forecast <- rep(0, (n - 1))
      FCFF_value <- rep(0, (n - 1))
      for (i in seq_len(n - 1)) {
      revenue_upstream <- revenue_prediction(oil_price[i], barrels_sold[i])
      revenue_forecast[i] <- revenue_upstream + chemical_revenue[i] + downstream_revenue[i]
      revenue <- revenue_upstream + chemical_revenue[i] + downstream_revenue[i]
      FCFF <- future_cashflow_forecast(revenue,
                                 Modelconstants[1],
                                 Modelconstants[2],
                                 Modelconstants[3])
      FCFF_value[i] <- FCFF
      PV_value <- PV_calculation(FCFF, Modelconstants[4], i)
      PV_vector[i] <- PV_value
      years[i] <- i
                                 }
    return(data.frame(years, revenue_forecast, FCFF_value, PV_vector))
  }

terminal_value <- function(oil_price, barrels_sold, chemical_revenue, downstream_revenue, n, Modelconstants)
  { revenue_upstream <- revenue_prediction(oil_price[n], barrels_sold[n])
    revenue <- revenue_upstream + chemical_revenue[n] + downstream_revenue[n]
    FCFF <- future_cashflow_forecast(revenue, Modelconstants[1], Modelconstants[2], Modelconstants[3])
    #print(paste0("This is FCFF for terminal year ", FCFF))
    terminal_growth_rate <- ((oil_price[n] * barrels_sold[n]) - (oil_price[n - 1] * barrels_sold[n -
                                                                             1])) / ((oil_price[n - 1] * barrels_sold[n - 1]))
    #print(paste0("This is the terminal growth rate ", terminal_growth_rate))
    calc_value <- terminal_value_calculation(FCFF, Modelconstants[4], terminal_growth_rate)
    #print(paste0("Terminal cashflow is ", calc_value))
    return(calc_value)
  }

enterprise_value <-
  function(terminal_value,
           total_PV_forecast,
           Modelconstants,
           n) {
    calc_value <-
      (PV_calculation(terminal_value, Modelconstants[4], (n - 1)) +
         total_PV_forecast)
    #print(paste0("This is the PV of terminal cashflow ", (PV_calculation(terminal_value, Modelconstants[4], (n-1)))))
    #print(paste0("This is the enterprise value ", calc_value))
    return(calc_value)
  }
###share_price takes balancesheet which is a vector with debt, minority interests, cash,
### non-operating assets (where debit balances are positive and credit balances are negative)
share_price <-
  function(enterprise_value,
           balancesheet,
           number_of_shares) {
    equity_value <- enterprise_value + sum(balancesheet)
    estimated_share_price <- equity_value / number_of_shares
    return(estimated_share_price)
  }
####End of Forecast functions####

####Wrapper Function####
wrapped_share_price <-
  function(oil_price, barrel_sold, chemical_revenue, downstream_revenue, Modelconstants, balancesheet, n, no_of_shares)
    {total_PV_forecast_result <-
      total_PV_forecast(oil_price,
                        barrel_sold,
                        chemical_revenue,
                        downstream_revenue,
                        n,
                        Modelconstants)
    terminal_value_result <-
      terminal_value(oil_price,
                     barrel_sold,
                     chemical_revenue,
                     downstream_revenue,
                     n,
                     Modelconstants)
    enterprise_value_result <-
      enterprise_value(terminal_value_result,
                       total_PV_forecast_result,
                       Modelconstants,
                       n)
    share_price_result <-
      share_price(enterprise_value_result, balancesheet, no_of_shares)
    return(share_price_result)
  }
