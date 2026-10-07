#' Calculate the time elapsed, t, given two datetime objects
#'
#' @param t1 initial datetime
#' @param t2 final datetime
#' 
#' @return Returns a numeric for time elapsed, t
#' 
#' @author Nicole Torrie
#' 
#' @export
#'

# Note, datetimes must be given in the format: "yyyy-mm-dd hh:mm" e.g. t1 = "2026-07-03 9:15"

al_calculate_time_t <- function(t1, t2) {
  t_hours = as.numeric(difftime(t1, t2, units = "hours"))
  t = t_hours / 24
  t
}



