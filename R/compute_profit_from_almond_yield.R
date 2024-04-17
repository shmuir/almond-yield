#' Compute Profit from Almond Yield
#'
#' @param price price of almonds in ton/acre, default = $4,063
#' @param yield yield of almonds in tons / acre, default = 0.9 tons / acre
#' @param discount rate, default = 0.12
#' @param cost cost of growing almonds in dollars / ton / acre, default = $4000
#' @param acres acres being used for almond growing, default = 50 acres
#'
#' @return data frame with estimate of profit from almonds

compute_profit_from_almond_yield = function(price = 4063, yield = 0.9, discount = 0.12, cost = 4000, acres = 50){
  
  # generate a unique identifier or scenario number
  scen = seq(from=1, to=length(yield))
  
  # run almond_yield_anomaly function and check output
  yield_anomaly <- almond_yield_anomaly(clim)
    
  yearprofit = data.frame(scen=scen,
                          yield=yield,
                          year=yield_anomaly$year,
                          yield_anomaly = yield_anomaly$yield_anomaly)
  
  yearprofit$net = ((yearprofit$yield+yearprofit$yield_anomaly)*price - cost)*acres
  
  # note how discount is passed through to this function
  # remember to normalize the year to start year e.g the first year
  yearprofit = yearprofit %>% 
    mutate(netpre = compute_NPV(value=net, time=year-year[1], discount=discount ))
  
  return(list(annual_profit=yearprofit[,c("year", "net")], mean=mean(yearprofit$net)))
}
