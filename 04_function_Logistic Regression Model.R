#############################################
#### Logistic regression model fitting function for simulated data
#### Author: Kayla Vanderkruk
#### Date Created: April 9, 2021
#### Last Updated: April 9, 2021
#############################################

#### Logistic regression model fitting function for simulated data ####
# Inputs: Data set with lagged absenteeism data
#   , regression formulas to be fit (1 for each lag)
#   , and the lags being assessed
# Outputs:
#   mod - lists of linear models
#     mod is a list of lists.
#     Within each list is a list of yearly models created using all data that temporaly preceded that year (ie list length equal to number of years)
#     length of the mod list is the number of models fitted (ie number of lags)
#   resp - list of model responses
#     within each list is the responses for each year.
#     length of the list is the number of models fitted (ie number of lags)
log.reg <- function(lagdata, regformulas, lags = seq.int(1,15)){
  
  # Create lists of training and prediction datasets
  #   Training datasets - each year uses all data that temporally preceded that year
  train <- list()
  pred <- list()
  for(yr in unique(lagdata$ScYr)[-1]){ # no predictions for the first year, it is only used for model training
    train[[yr]] <- lagdata[lagdata$ScYr < yr,]
    pred[[yr]] <- lagdata[which(lagdata$ScYr == yr),]
  }
  
  # Fit a model for each lag value and each year, then predict response
  mod <- rep(list(list()), length(lags))
  resp <- list()
  for(i in seq_along(lags)){
    for(yr in unique(lagdata$ScYr)[-1]){ # remove the first year since it was only used for training
      mod[[i]][[yr]] <- glm(regformulas[[i]], data = train[[yr]], family = "binomial") # train model
      tmp.resp <- data.frame(resp = predict(mod[[i]][[yr]], pred[[yr]], type = "response")) # predict
      
      if(yr == unique(lagdata$ScYr)[2]){ #fixes error if trying to rbind first element in list
        resp[[i]] <- tmp.resp
      } else {
        resp[[i]] <- rbind(resp[[i]], tmp.resp)
      }
    } 
  }
  return(list(mod = mod, resp = resp))
}

