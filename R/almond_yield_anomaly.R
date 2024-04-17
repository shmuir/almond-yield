#' Calculating Almond Yield Anomaly
#'
#' #' @param  clim climate data frame
#' as daily (day, month, year, tmin_C, tmax_C, precip )
#' @param  Tmincoeff1 default=-0.015
#' @param  Tmaxcoeff2 default=-0.0046
#' @param  Pcoeff1 default=-0.07
#' @param  Pcoeff2 default=0.0043
#'
#' @return minimum yield anomaly, maximum yield anomaly, mean yield anomaly; ton(s) per acre
#'
#' @examples
#' almond_yield_anomaly(clim)
#' 
#' @references 
#' doi:10.1016/j.agrformet.2006.10.006

almond_yield_anomaly <- function(clim, Tmincoeff1=-0.015, Tmincoeff2=-0.0046, Pcoeff1=-0.07, Pcoeff2=0.0043, intercep=0.28) {
  
  # pull out minimum february temperatures from each year
  feb_tmin_c <- clim %>%
    group_by(year) %>%
    filter(month == 2) %>% # select just feb
    summarise(feb_tmin_c = min(tmin_c)) # get the min
  
  # calculate total january precipitation for each year
  jan_precip_mm <- clim %>%
    group_by(year) %>%
    filter(month == 1) %>% # select just jan
    summarise(jan_precip_mm = sum(precip)) # get the sum
  
  # calculate almond yield 
  yield_anomaly <- full_join(feb_tmin_c, jan_precip_mm) %>%
    mutate(yield_anomaly = Tmincoeff1*feb_tmin_c + Tmincoeff2*feb_tmin_c^2 + Pcoeff1*jan_precip_mm + Pcoeff2*jan_precip_mm^2 + intercep) # calc based on equation from lobell et al. 2006
  
  return(yield_anomaly)
  
}

