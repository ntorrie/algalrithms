#' Calculate the growth rate of a phytoplankton
#'
#' @param E0 xxxx
#' @param Kd xxxx
#' @param Z depth 
#'
#' @return Returns a ggplot object of Salinity validation data colorized based on SAL flag
#' 
#' @author Nicole Torrie
#' 
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual geom_line geom_ribbon labs
#' 
#' @export
#'


# Ez = light intensity at a depth "z"
# Kz = Diffuse Attenuation Coefficient: how rapidsly light is attenuated. Higher Kz = greater opacity
# E0 = light intensity at the surface

E0 <- 1840
Kd <- 0.37
Z <- 15

al_light_intensity <- function(E0,
                               Kd,
                               Z) {
Ez <- E0 * e^(-Kd*z)
Ez
}


