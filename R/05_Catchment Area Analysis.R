#############################################
#### Fit logistic regression models to each simulation series/replication
#### , calculation evaluation metrics based on alarms raised by models for each series
#### , and select lag and threshold values based on metrics
#### For the Catchment Area data set
#### Author: Kayla Vanderkruk
#### Date Created: April 9, 2021
#### Last Updated: April 9, 2021
#############################################

#### Load functions and data ####
source("R/04_function_Logistic Regression Model.R")
source("R/04_function_evaluation metrics and model selection.R")

load("Data/simulated individuals.RData")
load("Data/simulated model data.RData")

#### Regression Formulas ####
# One formula for each considered lag value
lagform2a <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14+lag15)
lagform2b <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14)
lagform2c <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13)
lagform2d <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12)
lagform2e <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11)
lagform2f <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10)
lagform2g <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9)
lagform2h <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8)
lagform2i <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7)
lagform2j <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6)
lagform2k <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5)
lagform2l <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3+lag4)
lagform2m <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2+lag3)
lagform2n <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1+lag2)
lagform2o <- as.formula(Case ~ catchID + sinterm + costerm + lag0+lag1)
forms <- list(lagform2o,lagform2n,lagform2m,lagform2l,lagform2k,lagform2j,lagform2i,lagform2h,lagform2g,lagform2f,lagform2e,lagform2d,lagform2c,lagform2b,lagform2a)

# lag and threshold values
l <- seq.int(1,15)
thres <- seq(0.1,0.6,by = 0.05)

#### Fit Logisitc Regression Models to Lagged Absenteeism Data and Predict Start of Influenza Season ####
# This function uses the 1st year of data for training, and models are refit annually
# to include data from all prior years
# ie year 2 predictions are based on a model that was fit with year 1 data
#   year 3 predictions are based on a model that was fit with year 1 and 2 data,...
#   year 10 predictions are based on a model that was fit using years 1-9 data.
s1.catchment.reg <- log.reg(lagdata = series1.master$catchment, regformulas = forms, lags = l)
s2.catchment.reg <- log.reg(lagdata = series2.master$catchment, regformulas = forms, lags = l)
s3.catchment.reg <- log.reg(lagdata = series3.master$catchment, regformulas = forms, lags = l)
s4.catchment.reg <- log.reg(lagdata = series4.master$catchment, regformulas = forms, lags = l)
s5.catchment.reg <- log.reg(lagdata = series5.master$catchment, regformulas = forms, lags = l)
s6.catchment.reg <- log.reg(lagdata = series6.master$catchment, regformulas = forms, lags = l)
s7.catchment.reg <- log.reg(lagdata = series7.master$catchment, regformulas = forms, lags = l)
s8.catchment.reg <- log.reg(lagdata = series8.master$catchment, regformulas = forms, lags = l)
s9.catchment.reg <- log.reg(lagdata = series9.master$catchment, regformulas = forms, lags = l)
s10.catchment.reg <- log.reg(lagdata = series10.master$catchment, regformulas = forms, lags = l)

#### SAVE RESULTS ####
save(s1.catchment.reg 
     # , s2.catchment.reg 
     # , s3.catchment.reg 
     # , s4.catchment.reg 
     # , s5.catchment.reg 
     # , s6.catchment.reg 
     # , s7.catchment.reg 
     # , s8.catchment.reg 
     # , s9.catchment.reg 
     # , s10.catchment.reg
     , file = "Results/catchment area logreg models.RData")

#### calc model evaluation metrics ####
weights <- c(1:9)/sum(c(1:9)) #yearly weights for WAATQ and WFATQ, number of full years in each years model, divided by the total number of years in all yearly models

s1.catchment.metric <- eval.metrics(lagdata = series1.master$catchment, mod_resp = s1.catchment.reg$resp
                                 , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s2.catchment.metric <- eval.metrics(lagdata = series2.master$catchment, mod_resp = s2.catchment.reg$resp
                                 , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s3.catchment.metric <- eval.metrics(lagdata = series3.master$catchment, mod_resp = s3.catchment.reg$resp
                                 , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s4.catchment.metric <- eval.metrics(lagdata = series4.master$catchment, mod_resp = s4.catchment.reg$resp
                                 , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s5.catchment.metric <- eval.metrics(lagdata = series5.master$catchment, mod_resp = s5.catchment.reg$resp
                                 , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s6.catchment.metric <- eval.metrics(lagdata = series6.master$catchment, mod_resp = s6.catchment.reg$resp
                                 , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s7.catchment.metric <- eval.metrics(lagdata = series7.master$catchment, mod_resp = s7.catchment.reg$resp
                                 , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s8.catchment.metric <- eval.metrics(lagdata = series8.master$catchment, mod_resp = s8.catchment.reg$resp
                                 , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s9.catchment.metric <- eval.metrics(lagdata = series9.master$catchment, mod_resp = s9.catchment.reg$resp
                                 , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s10.catchment.metric <- eval.metrics(lagdata = series10.master$catchment, mod_resp = s10.catchment.reg$resp
                                  , type = "c", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)

#### SAVE RESULTS ####
save(s1.catchment.metric, 
     s2.catchment.metric, 
     s3.catchment.metric, 
     s4.catchment.metric, 
     s5.catchment.metric, 
     s6.catchment.metric, 
     s7.catchment.metric, 
     s8.catchment.metric, 
     s9.catchment.metric, 
     s10.catchment.metric
     , file = "Results/catchment area logreg evaluation metric results.RData")

#### Save metric selected models for all series/replications ####

all.series.AATQ <- rbind(cbind(s1.catchment.metric$best.AATQ, series=1), 
                         cbind(s2.catchment.metric$best.AATQ, series=2),
                         cbind(s3.catchment.metric$best.AATQ, series=3),
                         cbind(s4.catchment.metric$best.AATQ, series=4),
                         cbind(s5.catchment.metric$best.AATQ, series=5),
                         cbind(s6.catchment.metric$best.AATQ, series=6),
                         cbind(s7.catchment.metric$best.AATQ, series=7),
                         cbind(s8.catchment.metric$best.AATQ, series=8),
                         cbind(s9.catchment.metric$best.AATQ, series=9),
                         cbind(s10.catchment.metric$best.AATQ, series=10))

all.series.FATQ <- rbind(cbind(s1.catchment.metric$best.FATQ, series=1), 
                         cbind(s2.catchment.metric$best.FATQ, series=2),
                         cbind(s3.catchment.metric$best.FATQ, series=3),
                         cbind(s4.catchment.metric$best.FATQ, series=4),
                         cbind(s5.catchment.metric$best.FATQ, series=5),
                         cbind(s6.catchment.metric$best.FATQ, series=6),
                         cbind(s7.catchment.metric$best.FATQ, series=7),
                         cbind(s8.catchment.metric$best.FATQ, series=8),
                         cbind(s9.catchment.metric$best.FATQ, series=9),
                         cbind(s10.catchment.metric$best.FATQ, series=10))

all.series.WAATQ <- rbind(cbind(s1.catchment.metric$best.WAATQ, series=1), 
                          cbind(s2.catchment.metric$best.WAATQ, series=2),
                          cbind(s3.catchment.metric$best.WAATQ, series=3),
                          cbind(s4.catchment.metric$best.WAATQ, series=4),
                          cbind(s5.catchment.metric$best.WAATQ, series=5),
                          cbind(s6.catchment.metric$best.WAATQ, series=6),
                          cbind(s7.catchment.metric$best.WAATQ, series=7),
                          cbind(s8.catchment.metric$best.WAATQ, series=8),
                          cbind(s9.catchment.metric$best.WAATQ, series=9),
                          cbind(s10.catchment.metric$best.WAATQ, series=10))

all.series.WFATQ <- rbind(cbind(s1.catchment.metric$best.WFATQ, series=1), 
                          cbind(s2.catchment.metric$best.WFATQ, series=2),
                          cbind(s3.catchment.metric$best.WFATQ, series=3),
                          cbind(s4.catchment.metric$best.WFATQ, series=4),
                          cbind(s5.catchment.metric$best.WFATQ, series=5),
                          cbind(s6.catchment.metric$best.WFATQ, series=6),
                          cbind(s7.catchment.metric$best.WFATQ, series=7),
                          cbind(s8.catchment.metric$best.WFATQ, series=8),
                          cbind(s9.catchment.metric$best.WFATQ, series=9),
                          cbind(s10.catchment.metric$best.WFATQ, series=10))

all.series.FAR <- rbind(cbind(s1.catchment.metric$best.FAR, series=1), 
                        cbind(s2.catchment.metric$best.FAR, series=2),
                        cbind(s3.catchment.metric$best.FAR, series=3),
                        cbind(s4.catchment.metric$best.FAR, series=4),
                        cbind(s5.catchment.metric$best.FAR, series=5),
                        cbind(s6.catchment.metric$best.FAR, series=6),
                        cbind(s7.catchment.metric$best.FAR, series=7),
                        cbind(s8.catchment.metric$best.FAR, series=8),
                        cbind(s9.catchment.metric$best.FAR, series=9),
                        cbind(s10.catchment.metric$best.FAR, series=10))

all.series.ADD <- rbind(cbind(s1.catchment.metric$best.ADD, series=1), 
                        cbind(s2.catchment.metric$best.ADD, series=2),
                        cbind(s3.catchment.metric$best.ADD, series=3),
                        cbind(s4.catchment.metric$best.ADD, series=4),
                        cbind(s5.catchment.metric$best.ADD, series=5),
                        cbind(s6.catchment.metric$best.ADD, series=6),
                        cbind(s7.catchment.metric$best.ADD, series=7),
                        cbind(s8.catchment.metric$best.ADD, series=8),
                        cbind(s9.catchment.metric$best.ADD, series=9),
                        cbind(s10.catchment.metric$best.ADD, series=10))

write.csv(all.series.AATQ, file = "Results/catchment_AATQ_selected.csv", row.names = FALSE)
write.csv(all.series.FATQ, file = "Results/catchment_FATQ_selected.csv", row.names = FALSE)
write.csv(all.series.WAATQ, file = "Results/catchment_WAATQ_selected.csv", row.names = FALSE)
write.csv(all.series.WFATQ, file = "Results/catchment_WFATQ_selected.csv", row.names = FALSE)
write.csv(all.series.FAR, file = "Results/catchment_FAR_selected.csv", row.names = FALSE)
write.csv(all.series.ADD, file = "Results/catchment_ADD_selected.csv", row.names = FALSE)
