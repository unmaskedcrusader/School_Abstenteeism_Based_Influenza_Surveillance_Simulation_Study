library(EpiILM)
library(splitstackshape)
set.seed(2021-04-03)

load(file = "Data/simulated individuals.RData")

individuals <- individuals[sample(nrow(individuals),1000),]

#### Discard unreasonable epidemics ####

# Function: Calculate infection rate
sim.epi.infection.rate <- function(epidata){
  num.infected <- sum(epidata$inftime > 0)
  num.atrisk <- length(epidata$inftime)
  
  infection.rate <- num.infected / num.atrisk
  return(infection.rate)
}



#### Function to simulate 12 epidemics ####
# simulate 2 extra epidemics incase we need to toss some due to unrealistic spread

sim12epiNew <- function(b, sus, spar, num_inf){
  
  #par(mfcol=c(3,3))
  
  runs <- list()
  num_catchment <- length(unique(individuals$catchID))
  
  for(i in 1:10){
    
    # Select individuals from each catchment area for initial infections.
    #   Number of initial infections per cathcment area = numinf.catch
    sample <- stratified(individuals, "catchID", size = num_inf)$individualID
    
    # Set Initial infection time for the sampled individuals
    #   Start is the "epidemic start time"
    #   Individuals are randomly infected within 14 days after the start time 
    first.inf <- rep(0, nrow(individuals))
    start <- max(round(rnorm(1, mean = 45, sd=15)), 20)
    first.inf[individuals$individualID %in% sample] <- start + floor(runif(num_inf*num_catchment, 0, 15))
    
    # simulate epidemic
    SIR <- epidata(type="SIR"
                   , n=nrow(individuals)
                   , tmin=1
                   , tmax=270
                   , sus.par= sus
                   , beta= b
                   , spark = spar
                   , x=individuals$loc.x*2
                   , y=individuals$loc.y*2
                   , inftime = first.inf
                   , infperiod=rep(4, nrow(individuals)))
    

    # calculate infection rate by taking ratio of sum of infection time to 
    # total number of infections
    infection.rate <- sum(SIR$inftime > 0) / length(SIR$inftime)
    
    if(infection.rate <= 0.02){
      
      SIR <- epidata(type="SIR"
                     , n=nrow(individuals)
                     , tmin=1
                     , tmax=270
                     , sus.par= sus
                     , beta= b
                     , spark = spar
                     , x=individuals$loc.x*2
                     , y=individuals$loc.y*2
                     , inftime = first.inf
                     , infperiod=rep(4, nrow(individuals)))
    }
    
    # plot(SIR, plottype = "curve", curvetype = "newinfect")
    
    runs[[i]]<- list(XYcoordinates = SIR$XYcoordinates, inftime = SIR$inftime, remtime = SIR$remtime, first.inf = first.inf, start = start)
  }
  
  #par(mfcol=c(1,1))
  
  
  # # Calculate infection rates for each year in each series
  # infect_rate <- unlist(lapply(runs, FUN = sim.epi.infection.rate))
  # 
  # 
  # 
  # for(i in length(infect_rate)){
  #   
  #   if(infect_rate[i] < 0.02){
  #     runs[i] <- NULL
  #   }
  # }
  # 
  # if(length(infect_rate)10){
  #   runs <- sample(runs, 10)
  # } els
    
  return(runs)
  
}

#### Simulate 10 replications or series ####

epi_series1 <- sim12epiNew(b=3, sus=.0019, spar=0,  2)
epi_series2 <- sim12epi(b=3, sus=.0019, spar=0, numinf.catch = 2)
epi_series3 <- sim12epi(b=3, sus=.0019, spar=0, numinf.catch = 2)
epi_series4 <- sim12epi(b=3, sus=.0019, spar=0, numinf.catch = 2)
epi_series5 <- sim12epi(b=3, sus=.0019, spar=0, numinf.catch = 2)
epi_series6 <- sim12epi(b=3, sus=.0019, spar=0, numinf.catch = 2)
epi_series7 <- sim12epi(b=3, sus=.0019, spar=0, numinf.catch = 2)
epi_series8 <- sim12epi(b=3, sus=.0019, spar=0, numinf.catch = 2)
epi_series9 <- sim12epi(b=3, sus=.0019, spar=0, numinf.catch = 2)
epi_series10 <- sim12epi(b=3, sus=.0019, spar=0, numinf.catch = 2)



# Calculate infection rates for each year in each series
epi.inf.rate1 <- unlist(lapply(epi_series1, FUN = sim.epi.infection.rate))
epi.inf.rate2 <- unlist(lapply(epi_series2, FUN = sim.epi.infection.rate))
epi.inf.rate3 <- unlist(lapply(epi_series3, FUN = sim.epi.infection.rate))
epi.inf.rate4 <- unlist(lapply(epi_series4, FUN = sim.epi.infection.rate))
epi.inf.rate5 <- unlist(lapply(epi_series5, FUN = sim.epi.infection.rate))
epi.inf.rate6 <- unlist(lapply(epi_series6, FUN = sim.epi.infection.rate))
epi.inf.rate7 <- unlist(lapply(epi_series7, FUN = sim.epi.infection.rate))
epi.inf.rate8 <- unlist(lapply(epi_series8, FUN = sim.epi.infection.rate))
epi.inf.rate9 <- unlist(lapply(epi_series9, FUN = sim.epi.infection.rate))
epi.inf.rate10 <- unlist(lapply(epi_series10, FUN = sim.epi.infection.rate))

# ATTN: Manual Step
# Discard year if infection rate is less than 2%. 
#   If this does not occur in the first 10 years,
#   remove the last 2 replications so that each 
#   run has 10 years of data
#   If you need to discard more than 2 years from a 
#   series, simulate a new series.
#   You can find the infection rates in the 
#   epi.inf.rate output above
epi_series1[c(11,12)] <- NULL
epi_series2[c(1,9)]   <- NULL
epi_series3[c(7,12)]  <- NULL
epi_series4[c(3,10)]  <- NULL
epi_series5[c(5,11)]  <- NULL
epi_series6[c(1,11)]  <- NULL
epi_series7[c(11,12)] <- NULL
epi_series8[c(3,10)]  <- NULL
epi_series9[c(5,12)]  <- NULL
epi_series10[c(10,12)]<- NULL

#### SAVE DATA ####
save(epi_series1
     , epi_series2
     , epi_series3
     , epi_series4
     , epi_series5
     , epi_series6
     , epi_series7
     , epi_series8
     , epi_series9
     , epi_series10
     , file="Data/simulated epidemics.RData")
