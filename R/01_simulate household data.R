library(MASS)
library(splitstackshape)
set.seed(2021-03-21)

#### Initialize Population Parameters ####

#Use census population for guidance on population simulation parameters
#https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page.cfm?Lang=E&Geo1=CSD&Code1=3523008&Geo2=CD&Code2=3523&Data=Count&SearchText=guelph&SearchType=Begins&SearchPR=01&B1=All&TABID=1

#Proportion of households of sizes 1,2,3,4,5+ members
prop.household.size <- c(0.23246269, 0.34281716, 0.16091418, 0.16427239, 0.09953358) 

# Proportion of parents that are coupled, or lone parents.
prop.parent.lone <- 0.2331099 # lone parent
prop.parent.couple <- 0.7668901 # coupled parents

# Proportion of number of children by parent type
prop.children.couple <- c(0.3634045, 0.4329440, 0.2036515) # coupled parent with 1,2,3+ children
prop.children.lone <- c(0.5857832, 0.3071523, 0.1070645) # lone parent with 1,2,3+ children

# proportion of children that are elementary school age
prop.children.elem.age <- 0.4976825

# Proportion of households with children
prop.household.with.children <- 0.4277052
prop.household.without.children <- 1-prop.household.with.children

#### Create School Catchment Areas ####
#simulate 16 catchments, each with an area of 20x20

# Simulate number of schools within catchment area
# alpha and beta of gamma distribution was determined using actual data and the fitdistr() function. 
catchment.sizes <- round(rgamma(16, shape = 4.313320, rate = 3.026894))
catchment.sizes <- ifelse(catchment.sizes ==0, 1, catchment.sizes) # if 0 schools in catchment area, force to 1.

# Create Catchment area boundries (X and Y coordinates)
xstart <- c(rep(0, 4), rep(20,4), rep(40,4), rep(60,4))
xend <- c(rep(20, 4), rep(40,4), rep(60, 4), rep(80,4))
ystart <- rep(c(0,20,40,60), 4)
yend <- rep(c(20,40,60,80), 4)

sim.catchment.data <- data.frame(catchID = c(1:16)
                                 , num.schools = catchment.sizes
                                 , xstart, xend
                                 , ystart, yend)

#### Create Elementary Schools population size ####
# Simulate number of students for each school
# alpha and beta of gamma distribution was determined using actual data and the fitdistr() function. 
school.pop <- round(rgamma(sum(catchment.sizes), shape = 5.27426341, rate = 0.01427793))
school.id <- seq.int(length(school.pop))

# Assign schools to catchment areas
school.to.catchment.map <- c()
for(i in 1:nrow(sim.catchment.data)){
  school.to.catchment.map <- c(school.to.catchment.map, rep(sim.catchment.data$catchID[i], sim.catchment.data$num.schools[i]))
}

school.to.catchment.map <- data.frame(catchID = school.to.catchment.map
                                      , schoolID = school.id
                                      , schoolpop = school.pop)
sim.school.data <- merge(school.to.catchment.map, sim.catchment.data[,c("catchID", "xstart", "xend", "ystart", "yend")]
                         , by="catchID", all.y = TRUE)

#### Simulate Sub-Population 1: households with children ####

n1 <- sum(sim.school.data$schoolpop)*5 #not sure how many households are needed to meet school population sizes, so simulate more than needed.  Extra will be removed.

# Random number generation to determine parent type (lone or couple), 
#     number of children within household given parent type, and
#     if each child is elementary school age or not
unif.parent.type <- runif(n1)
unif.child.num <- runif(n1)
unif.childage1 <- runif(n1)
unif.childage2 <- runif(n1)
unif.childage3 <- runif(n1)

parent.type <- ifelse(unif.parent.type <= prop.parent.couple, 2, 1) # parent type: 2 = coupled parent, 1 = lone parent.

# simulate number of children, based on parent type.
num.child <- ifelse(parent.type ==2, # number of children if couple with children 
                    ifelse(unif.child.num <= prop.children.couple[1], 1,
                           ifelse(unif.child.num <= sum(prop.children.couple[1:2]),2,3))
                    , ifelse(unif.child.num <= prop.children.lone[1], 1, # number of children if lone-parent with children
                             ifelse(unif.child.num <= sum(prop.children.lone[1:2]), 2, 3)))

# Simulate if child is elementary school age
child1.elem.age <- ifelse(unif.childage1 <= prop.children.elem.age,1,0) 
child2.elem.age <- ifelse(unif.childage2 <= prop.children.elem.age,1,0)
child3.elem.age <- ifelse(unif.childage3 <= prop.children.elem.age,1,0)
num.elem.child <- ifelse(num.child == 3, child1.elem.age + child2.elem.age + child3.elem.age
                        , ifelse(num.child == 2, child1.elem.age + child2.elem.age, child1.elem.age))

house.with.children <- data.frame(houseID = seq.int(length(parent.type))
                                  , num.parent = parent.type
                                  , num.child = num.child
                                  , num.elem.child = num.elem.child
                                  , schoolID = rep(0, length(parent.type)))

# Assign elementary aged children to elementary schools 
#     such that the school population is met
start <- 0
stop <-0
school.assignment <- c()
for(i in 1:nrow(sim.school.data)){
  start <- start + stop
  tmp.house <- house.with.children[(start+1):nrow(house.with.children),] #households remaining to be assigned
  cum.sum.students <- cumsum(tmp.house$num.elem.child) #cumulative sum of elementary aged children yet to be assigned
  stop <- which(cum.sum.students >= sim.school.data$schoolpop[i])[1] # find the row in which the school population is satisfied
  school.assignment <- c(school.assignment, rep(sim.school.data$schoolID[i], stop)) # assign all those households to that school
}

house.with.children <- house.with.children[1:length(school.assignment),] #keep all assigned households.  We had to simulate extra since we didn't know how many families would be needed.
house.with.children$schoolID <- school.assignment

# TEST: Check that the number of elementary school kids assigned to each school is similar to school population 
#     (may be 1-2 over because all children in a household should be assigned to the same school)
cbind(aggregate(house.with.children$num.elem.child ~ house.with.children$schoolID, FUN="sum"), sim.school.data$schoolpop) 

#update school populations to include the possible extra 1-2 children
sim.school.data$schoolpop <- aggregate(house.with.children$num.elem.child ~ house.with.children$schoolID, FUN="sum")[,2]

# include catchment area information into household data frame
house.with.children <- merge(house.with.children, sim.school.data, by = "schoolID")
house.with.children$num.people <- house.with.children$num.child + house.with.children$num.parent


#### Simulate Sub-Population 2: households without children ####

# Calculate the household size proportions for households without children.  
#     Based on the overall household size proportions from the census, 
#     the proportion of households with children, and the size distribution 
#     of households with children.
prop.household.size.nochildren <- c(prop.household.size[1]/prop.household.without.children) # proportion households of size 1
for(i in 1:4){ # calculate proportion of households of sizes 2-5
  y <- (prop.household.size[i+1]-prop.household.with.children*table(house.with.children$num.people)[i]/nrow(house.with.children))/prop.household.without.children
  prop.household.size.nochildren <- c(prop.household.size.nochildren, y)
}

# TEST: Check that the proportions sum to 1.
sum(prop.household.size.nochildren)

# Calculate the number of households without children per catchment area.
#     Based on the proportion of households with children.

catchment.household.count <- data.frame(table(house.with.children$catchID))
names(catchment.household.count) <- c("catchID", "num.household.with.children")
catchment.household.count$total.households <- round(catchment.household.count$num.household.with.children/prop.household.with.children)
catchment.household.count$num.household.no.children <- catchment.household.count$total.households - catchment.household.count$num.household.with.children

n2 <- sum(catchment.household.count$num.household.no.children)

# Simulate household size for households without children
unif.household.size <- runif(n2)

household.size <- ifelse(unif.household.size <= prop.household.size.nochildren[1], 1,  
                                     ifelse(unif.household.size <= sum(prop.household.size.nochildren[1:2]), 2
                                            , ifelse(unif.household.size <= sum(prop.household.size.nochildren[1:3]), 3
                                                     , ifelse(unif.household.size <= sum(prop.household.size.nochildren[1:4]), 4, 5))))


house.no.children <- data.frame(houseID = nrow(house.with.children) + seq.int(n2)
                                , num.people = household.size
                                , catchID = rep(0, n2))

# Assign households without children to catchment areas
stop <- 0
for(i in 1:nrow(catchment.household.count)){
  start <- stop + 1
  stop <- cumsum(catchment.household.count$num.household.no.children)[i]
  house.no.children$catchID[start:stop] <- catchment.household.count$catchID[i]
}

house.no.children <- merge(house.no.children, sim.catchment.data[,-2], by="catchID")

# TEST: check that the number of households without children per catchment area is as expected
table(house.no.children$catchID) == catchment.household.count$num.household.no.children


#### Combine Sub population 1 and 2 Households ####

house.no.children$schoolID <- 0
house.no.children$num.elem.child <- 0

households1 <- house.with.children[,c("houseID", "catchID", "schoolID", "num.people", "num.elem.child", "xstart", "xend", "ystart", "yend")]
households2 <- house.no.children[,c("houseID", "catchID", "schoolID", "num.people", "num.elem.child", "xstart", "xend", "ystart", "yend")]

households <- rbind(households1, households2)

# Generate house locations within catchment areas
households$loc.x <- runif(nrow(households), households$xstart, households$xend)
households$loc.y <- runif(nrow(households), households$ystart, households$yend)

#### Create individuals data frame (one row per individual)  ####

individuals <- expandRows(households, "num.people", drop=FALSE)
rownames(individuals) <- c() #remove row names
individuals$individualID <- seq.int(nrow(individuals))

# Create elementary school child indicator
individuals$elem.child.ind <- 0 
for(i in unique(individuals$houseID)){
  num.elem <- individuals[which(individuals$houseID == i), "num.elem.child"][1]
  if(num.elem > 0){
    individuals[which(individuals$houseID == i),][1:num.elem, "elem.child.ind"] <- 1
  }
}

# TEST: make sure the number of elem.child.ind is equal to the number of elementary school children
sum(individuals$elem.child.ind) == sum(households$num.elem.child)

individuals <- households[rep(row.names(households), households$num.people),]

### SAVE DATA ####
save(individuals, file = "Data/simulated individuals.RData")
save(households, file = "Data/simulated households.RData")
