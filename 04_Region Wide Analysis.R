#############################################
#### Fit logistic regression models to each simulation series/replication
#### , calculation evaluation metrics based on alarms raised by models for each series
#### , and select lag and threshold values based on metrics
#### For the Region Wide data set
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
lagform1a <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14+lag15)
lagform1b <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13+lag14)
lagform1c <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12+lag13)
lagform1d <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11+lag12)
lagform1e <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10+lag11)
lagform1f <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+lag10)
lagform1g <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9)
lagform1h <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8)
lagform1i <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6+lag7)
lagform1j <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5+lag6)
lagform1k <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4+lag5)
lagform1l <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3+lag4)
lagform1m <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2+lag3)
lagform1n <- as.formula(Case ~ sinterm + costerm + lag0+lag1+lag2)
lagform1o <- as.formula(Case ~ sinterm + costerm + lag0+lag1)
forms <- list(lagform1o,lagform1n,lagform1m,lagform1l,lagform1k,lagform1j,lagform1i,lagform1h,lagform1g,lagform1f,lagform1e,lagform1d,lagform1c,lagform1b,lagform1a)

# lag and threshold values
l <- seq.int(1,15)
thres <- seq(0.1,0.6,by = 0.05)

#### Fit Logisitc Regression Models to Lagged Absenteeism Data and Predict Start of Influenza Season ####
# This function uses the 1st year of data for training, and models are refit annually
# to include data from all prior years
# ie year 2 predictions are based on a model that was fit with year 1 data
#   year 3 predictions are based on a model that was fit with year 1 and 2 data,...
#   year 10 predictions are based on a model that was fit using years 1-9 data.
s1.region.reg <- log.reg(lagdata = series1.master$region, regformulas = forms, lags = l)
s2.region.reg <- log.reg(lagdata = series2.master$region, regformulas = forms, lags = l)
s3.region.reg <- log.reg(lagdata = series3.master$region, regformulas = forms, lags = l)
s4.region.reg <- log.reg(lagdata = series4.master$region, regformulas = forms, lags = l)
s5.region.reg <- log.reg(lagdata = series5.master$region, regformulas = forms, lags = l)
s6.region.reg <- log.reg(lagdata = series6.master$region, regformulas = forms, lags = l)
s7.region.reg <- log.reg(lagdata = series7.master$region, regformulas = forms, lags = l)
s8.region.reg <- log.reg(lagdata = series8.master$region, regformulas = forms, lags = l)
s9.region.reg <- log.reg(lagdata = series9.master$region, regformulas = forms, lags = l)
s10.region.reg <- log.reg(lagdata = series10.master$region, regformulas = forms, lags = l)

#### SAVE RESULTS ####
save(s1.region.reg, 
     s2.region.reg, 
     s3.region.reg, 
     s4.region.reg, 
     s5.region.reg, 
     s6.region.reg, 
     s7.region.reg, 
     s8.region.reg, 
     s9.region.reg, 
     s10.region.reg
     , file = "Results/region wide logreg models.RData")

#### Calculate Model Evaluation Metrics and Model Selection ####
weights <- c(1:9)/sum(c(1:9)) #yearly weights for WAATQ and WFATQ, number of full years in each years model divided by the total number of years in all yearly models

s1.region.metric <- eval.metrics(lagdata = series1.master$region, mod_resp = s1.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s2.region.metric <- eval.metrics(lagdata = series2.master$region, mod_resp = s2.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s3.region.metric <- eval.metrics(lagdata = series3.master$region, mod_resp = s3.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s4.region.metric <- eval.metrics(lagdata = series4.master$region, mod_resp = s4.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s5.region.metric <- eval.metrics(lagdata = series5.master$region, mod_resp = s5.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s6.region.metric <- eval.metrics(lagdata = series6.master$region, mod_resp = s6.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s7.region.metric <- eval.metrics(lagdata = series7.master$region, mod_resp = s7.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s8.region.metric <- eval.metrics(lagdata = series8.master$region, mod_resp = s8.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s9.region.metric <- eval.metrics(lagdata = series9.master$region, mod_resp = s9.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)
s10.region.metric <- eval.metrics(lagdata = series10.master$region, mod_resp = s10.region.reg$resp
                                , type = "r", l = l, thres = thres, ScYr = c(2:10), yr.weights = weights)

#### SAVE RESULTS ####
save(s1.region.metric, 
     s2.region.metric, 
     s3.region.metric, 
     s4.region.metric, 
     s5.region.metric, 
     s6.region.metric, 
     s7.region.metric, 
     s8.region.metric, 
     s9.region.metric, 
     s10.region.metric
     , file = "Results/region wide logreg evaluation metric results.RData")

#### Save metric selected models for all series/replications ####

all.series.AATQ <- rbind(cbind(s1.region.metric$best.AATQ, series=1), 
                         cbind(s2.region.metric$best.AATQ, series=2),
                         cbind(s3.region.metric$best.AATQ, series=3),
                         cbind(s4.region.metric$best.AATQ, series=4),
                         cbind(s5.region.metric$best.AATQ, series=5),
                         cbind(s6.region.metric$best.AATQ, series=6),
                         cbind(s7.region.metric$best.AATQ, series=7),
                         cbind(s8.region.metric$best.AATQ, series=8),
                         cbind(s9.region.metric$best.AATQ, series=9),
                         cbind(s10.region.metric$best.AATQ, series=10))

all.series.FATQ <- rbind(cbind(s1.region.metric$best.FATQ, series=1), 
                        cbind(s2.region.metric$best.FATQ, series=2),
                        cbind(s3.region.metric$best.FATQ, series=3),
                        cbind(s4.region.metric$best.FATQ, series=4),
                        cbind(s5.region.metric$best.FATQ, series=5),
                        cbind(s6.region.metric$best.FATQ, series=6),
                        cbind(s7.region.metric$best.FATQ, series=7),
                        cbind(s8.region.metric$best.FATQ, series=8),
                        cbind(s9.region.metric$best.FATQ, series=9),
                        cbind(s10.region.metric$best.FATQ, series=10))

all.series.WAATQ <- rbind(cbind(s1.region.metric$best.WAATQ, series=1), 
                         cbind(s2.region.metric$best.WAATQ, series=2),
                         cbind(s3.region.metric$best.WAATQ, series=3),
                         cbind(s4.region.metric$best.WAATQ, series=4),
                         cbind(s5.region.metric$best.WAATQ, series=5),
                         cbind(s6.region.metric$best.WAATQ, series=6),
                         cbind(s7.region.metric$best.WAATQ, series=7),
                         cbind(s8.region.metric$best.WAATQ, series=8),
                         cbind(s9.region.metric$best.WAATQ, series=9),
                         cbind(s10.region.metric$best.WAATQ, series=10))

all.series.WFATQ <- rbind(cbind(s1.region.metric$best.WFATQ, series=1), 
                         cbind(s2.region.metric$best.WFATQ, series=2),
                         cbind(s3.region.metric$best.WFATQ, series=3),
                         cbind(s4.region.metric$best.WFATQ, series=4),
                         cbind(s5.region.metric$best.WFATQ, series=5),
                         cbind(s6.region.metric$best.WFATQ, series=6),
                         cbind(s7.region.metric$best.WFATQ, series=7),
                         cbind(s8.region.metric$best.WFATQ, series=8),
                         cbind(s9.region.metric$best.WFATQ, series=9),
                         cbind(s10.region.metric$best.WFATQ, series=10))

all.series.FAR <- rbind(cbind(s1.region.metric$best.FAR, series=1), 
                        cbind(s2.region.metric$best.FAR, series=2),
                        cbind(s3.region.metric$best.FAR, series=3),
                        cbind(s4.region.metric$best.FAR, series=4),
                        cbind(s5.region.metric$best.FAR, series=5),
                        cbind(s6.region.metric$best.FAR, series=6),
                        cbind(s7.region.metric$best.FAR, series=7),
                        cbind(s8.region.metric$best.FAR, series=8),
                        cbind(s9.region.metric$best.FAR, series=9),
                        cbind(s10.region.metric$best.FAR, series=10))

all.series.ADD <- rbind(cbind(s1.region.metric$best.ADD, series=1), 
                        cbind(s2.region.metric$best.ADD, series=2),
                        cbind(s3.region.metric$best.ADD, series=3),
                        cbind(s4.region.metric$best.ADD, series=4),
                        cbind(s5.region.metric$best.ADD, series=5),
                        cbind(s6.region.metric$best.ADD, series=6),
                        cbind(s7.region.metric$best.ADD, series=7),
                        cbind(s8.region.metric$best.ADD, series=8),
                        cbind(s9.region.metric$best.ADD, series=9),
                        cbind(s10.region.metric$best.ADD, series=10))

write.csv(all.series.AATQ, file = "Results/region_AATQ_selected.csv", row.names = FALSE)
write.csv(all.series.FATQ, file = "Results/region_FATQ_selected.csv", row.names = FALSE)
write.csv(all.series.WAATQ, file = "Results/region_WAATQ_selected.csv", row.names = FALSE)
write.csv(all.series.WFATQ, file = "Results/region_WFATQ_selected.csv", row.names = FALSE)
write.csv(all.series.FAR, file = "Results/region_FAR_selected.csv", row.names = FALSE)
write.csv(all.series.ADD, file = "Results/region_ADD_selected.csv", row.names = FALSE)