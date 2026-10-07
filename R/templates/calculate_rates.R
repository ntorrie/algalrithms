# install the most recent algalrithms package version
#library(devtools)
#install_github("ntorrie/algalrithms", force = TRUE, dependencies = TRUE)
#library(algalrithms)

# or source all functions from within package working directory
library(miceadds) 
source.all("/Users/nicoletorrie/Documents/Dalhousie/MSc/r/algalrithms/R")

# load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


# bt = Biomass at given time
# b0 = Biomass at time zero (start)
# u = growth rate ()
# t = time (days)

# bt = b0*e^(ut)
# b0 = bt/e^(ut)
# u = ln(bt/b0)/t
# t = ln(bt/b0)/u

# set time params
t1 = "2026-07-03 9:15"
t2 = "2026-07-02 10:00"


## calculate bt
b0 <- 2.001
u <- 1.02
t <- al_calculate_time_t(t1, t2)

al_calculate_biomass_bt(b0, u, t)


## calculate b0
bt <- 17
u <- 1.02
t <- al_calculate_time_t(t1, t2)

al_calculate_biomass_b0(bt, u, t)


## calculate growth rate
b0 <- 2.001
bt <- 4.62
t <- al_calculate_time_t(t1, t2)
#t <- 1

al_calculate_growth_rate_u(b0, bt, t)














#Working with imported data (OLD CODE, TO BE RE-VAMPED)

#example
# read in growth data
growth_data <- read.csv("/Users/nicoletorrie/Documents/Dalhousie/MSc/primary_productivity_project/rbr_sensor_testing/260326_Chlam286/turner/growth_rate_calculation/chlam268_growth_data.csv")

# convert date time to proper r-readable format
growth_data$Datetime <- ymd_hm(growth_data$Datetime) 

# set your time interval
t1 = growth_data[1,1]
t2 = growth_data[2,1]
t3 = growth_data[3,1]

t_hours = as.numeric(difftime(t2, t1, units = "hours"))
t_days = t_hours / 24

# set your b0 and bt
b0 <- growth_data[2,2]
bt <- growth_data[3,2]

# b0 <- 9.04
# bt <- 22.73
# t_days <- 2

# calculate growth rate
u <- al_calculate_growth_rate_u(b0, bt, t_days)

#1-2 = 0.529
#2-3 = 0.92
#1-3 = 1.45
# new dailies tubes = 0.46


# calculate biomass
bt <- al_calculate_biomass_bt(b0, u, 2)


# projected growth for days 0-5
b0 <- growth_data[1,]

growth_projections <- data.frame(
  u = u,
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


