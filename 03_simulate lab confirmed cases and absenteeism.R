#### Load data and functions ####
source("R/03_functions_simulate lab confirmed cases and absenteeism.R")

load(file = "Data/simulated individuals.RData")
load(file = "Data/simulated epidemics.RData")

#### Create a master file for modelling ####
#   This includes simulating laboratory confirmed cases, and school absenteeism data sets
series1.master <- model.data(epi_series1, individuals)
series2.master <- model.data(epi_series2, individuals)
series3.master <- model.data(epi_series3, individuals)
series4.master <- model.data(epi_series4, individuals)
series5.master <- model.data(epi_series5, individuals)
series6.master <- model.data(epi_series6, individuals)
series7.master <- model.data(epi_series7, individuals)
series8.master <- model.data(epi_series8, individuals)
series9.master <- model.data(epi_series9, individuals)
series10.master <- model.data(epi_series10, individuals)

#### Exploration plots ####
plot.epi.info(series1.master$region, ScYr = 1) # region wide plot example
plot.epi.info(series1.master$catchment, ScYr = 1, catchment = 13) # catchment area plot example

#### SAVE DATA ####
save(series1.master,
     series2.master,
     series3.master,
     series4.master,
     series5.master,
     series6.master,
     series7.master,
     series8.master,
     series9.master,
     series10.master,
     file = "Data/simulated model data.RData")
