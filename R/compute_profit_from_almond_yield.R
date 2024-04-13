compute_profit_from_almond_yield = function(price, year, yield, discount){
  
  # make sure values are reasonable
  if (length(yield) < 1)
    return(NA)
  
  # energy cannot be negative
  if (min(yield ) < 0)
    return(NA)
  
  # generate a unique identifier or scenario number
  scen = seq(from=1, to=length(yield))
  yearprofit = data.frame(scen=scen, yield=yield, year=year)
  yearprofit$net =  yearprofit$yield*price
  
  # note how discount is passed through to this function
  # remember to normalize the year to start year e.g the first year
  yearprofit= yearprofit %>% 
    mutate(netpre = compute_NPV(value=net, time=year-year[1], discount=discount ))
  
  return(yearprofit)
}