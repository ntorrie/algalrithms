#' Calculate the growth rate of a phytoplankton
#'
#' @param b0 biomass at time zero
#' @param u growth rate
#' @param t time 
#'
#' @return Returns a biomass estimate for time t
#' 
#' @author Nicole Torrie
#' 
#' @export
#'


# calculate biomass function
al_calculate_biomass_bt <- function(b0, u, t) {
  bt = b0 * exp(u * t)
}


