# load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# bt = Biomass at given time
# b0 = Biomass at time zero (start)
# u = growth rate ()
# t = time (days)
# u = ln(bt/b0)/t
# bt = b0*e^(ut)
# t = ln(bt/b0)/u

# load functions
# growth rate function (u)
al_calculate_growth_rate_u <- function(b0, bt, t) {
  u <- log(bt / b0) / t
  u
}

# biomass function (bt)
al_calculate_biomass_bt <- function(b0, u, t) {
  bt = b0 * exp(u * t)
}


# read in growth data
growth_data <- read.csv("/Users/nicoletorrie/Documents/Dalhousie/MSc/primary_productivity_project/rbr_sensor_testing/260313_PseudP47/turner/growth_rate_calculation/pseud_p47_growth_data.csv")

# convert date time to proper r-readable format
growth_data$Datetime <- mdy_hm(growth_data$Datetime) 


# set your time interval
t1 = growth_data[1,1]
t2 = growth_data[2,1]

t_hours = as.numeric(difftime(t2, t1, units = "hours"))
t_days = t_hours / 24

# calculate growth rate
u <- al_calculate_growth_rate_u(1.89, 5.013, t_days)

# calculate biomass
bt <- al_calculate_biomass_bt(1.89, u, 2)


# projected growth for days 0-5
b0 <- growth_data[1,2]

growth_projections <- data.frame(
  Day = c(0:5)) %>%
  mutate(
  CorrF_Predict = al_calculate_biomass_bt(b0, u, Day)
)


# plot growth projections
ggplot(growth_projections) + 
  aes(x = Day, y = CorrF_Predict) + 
  geom_line(color = "darkgreen") +
  geom_point(size = 2) + 
  geom_label(aes(label = round(CorrF_Predict, digits = 3), nudge_y = 13))


