library(readr)
library(dplyr)
library(ggplot2)

#read in growth data
growth_data <- read.csv("/Users/nicoletorrie/Documents/Dalhousie/MSc/primary_productivity_project/rbr_sensor_testing/260313_PseudP47/turner/growth_rate_calculation/pseud_p47_growth_data.csv")

# bt = Biomass at given time
# b0 = Biomass at time zero (start)
# u = growth rate ()
# t = time (days)

# u = ln(bt/b0)/t
# 
# bt = b0*e^(ut)
# 
# t = ln(bt/b0)/u

# calculate growth rate function
al_calculate_growth_rate_u <- function(b0, bt, t) {
  u <- log(bt / b0) / t
  u
}


# calculate biomass function
al_calculate_biomass_bt <- function(b0, u, t) {
  bt = b0 * exp(u * t)
}


u <- al_calculate_growth_rate_u(1.89, 5.013, 1)

bt <- al_calculate_biomass_bt(1.89, u, 2)

b0 <- growth_data[1,2]


growth_projections <- data.frame(
  Day = c(0:5)) %>%
  mutate(
  CorrF_Predict = al_calculate_biomass_bt(b0, u, Day)
)


# plot growth projections
ggplot(growth_projections) + 
  aes(x = Day, y = CorrF_Predict) + 
  geom_line(color = "lightgreen") +
  geom_point() 


