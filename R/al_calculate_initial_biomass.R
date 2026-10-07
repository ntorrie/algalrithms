#' Calculate the initial biomass of a phytoplankton culture based on a given biomass and time, assuming exponential growth
#'
#' @param bt biomass at time t
#' @param u growth rate
#' @param t time elapsed
#'
#' @return Returns a biomass estimate for time t = 0
#' 
#' @author Nicole Torrie
#' 
#' @export
#'


# calculate biomass function
al_calculate_biomass_b0 <- function(bt, u, t) {
  b0 = bt / exp(u * t)
  b0
}



