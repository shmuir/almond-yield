#' Calculating Almond Yield Anomaly
#'
#' @param df a data frame containing year, month, tmin_c, precip
#'
#' @return minimum yield anomaly, maximum yield anomaly, mean yield anomaly; ton(s) per acre
#'
#' @examples
#' almond_yield_anomaly(df)
#' 
#' @references 
#' doi:10.1016/j.agrformet.2006.10.006

almond_yield_anomaly <- function(df) {
  
  # pull out minimum february temperatures from each year
  feb_tmin_c <- df %>%
    group_by(year) %>%
    filter(month == 2) %>% # select just feb
    summarise(feb_tmin_c = min(tmin_c)) # get the min
  
  # calculate total january precipitation for each year
  jan_precip_mm <- df %>%
    group_by(year) %>%
    filter(month == 1) %>% # select just jan
    summarise(jan_precip_mm = sum(precip)) # get the sum
  
  # calculate almond yield 
  yield_anomaly <- full_join(feb_tmin_c, jan_precip_mm) %>%
    mutate(yield_anomaly = -0.015*feb_tmin_c - 0.0046*feb_tmin_c^2 - 0.07*jan_precip_mm + 0.0043*jan_precip_mm^2 + 0.28) # calc based on equation from lobell et al. 2006
  
  # calculate the min, max, and mean yield over the whole time period
  min_yield <- min(yield_anomaly$yield_anomaly)
  max_yield <- max(yield_anomaly$yield_anomaly)
  mean_yield <- mean(yield_anomaly$yield_anomaly)
  
  # print the min, max, and mean
  return(print(paste("Minimum Yield:", round(min_yield, 2), "ton(s) per acre |",
                     "Maximum Yield:", round(max_yield, 2), "ton(s) per acre |",
                     "Mean Yield:", round(mean_yield, 2), "ton(s) per acre")))
  
}

