#' Calculate the growth rate of a phytoplankton
#'
#' @param b0 biomass at time zero
#' @param bt biomass at time t
#' @param t time 
#'
#' @return Returns an estimated growth rate
#' 
#' @author Nicole Torrie
#' 
#' @export
#'


# note, a minimum of two data points are needed to calculate the growth rate

al_calculate_growth_rate <- function(b0, bt, t) {
  u <- log(bt / b0) / t
  u
}


