 library(testthat)
 
 
 # Simulate households with children
 
 #### Simulating catchment areas ####
 
 # Function to simulate specified catchment square area (a x a)
 
 catchment_sim <- function(n, alpha, gamma, area){
   
   #gamma distribution of catchment area
   size <- round(rgamma(n, alpha, gamma)) |> 
     (\(x) replace(x, x == 0, 1))()
   
   
   # creating empty vectors to hold number catchment areas of size n
   xStart <- c()
   xEnd <- c()
   yStart <- c()
   yEnd <- c()
   
   
   # for loop that populates the start and end values for x and y coordinates of the boundaries of catchment areas
   for(i in 1:n){
     
     xStart[i] <- (i-1) %/% (n/4) * area
     xEnd[i] <- (i-1) %/% (n/4) * area + area
     yStart[i] <- (i-1) %% (n/4) * area
     yEnd[i] <- (i-1) %% (n/4) * area + area  
     
   }
   
   
   # populating a data frame with simulated data 
   sim_catchment_df <- data.frame(catchID = c(1:n), 
                                  num.schools = size,
                                  xStart, xEnd,
                                  yStart, yEnd)
   
   return(sim_catchment_df) 
 }
 
 
 
 #### Create Elementary Schools population size ####
 elementary_pop <- function(df, shape, rate){
   
   
   # gamma distribution of number of elementary schools
   school_pop <- round(rgamma(sum(df$num.schools), shape, rate))
   school_id <- seq.int(length(school_pop))
   
   
   school_to_catchment_map <- c()
   
   # for loop to assign simulated catchment areas to number of elementary schools in each area
   for(i in 1:nrow(df)){
     school_to_catchment_map <- c(school_to_catchment_map, rep(df$catchID[i], df$num.schools[i]))
   }
   
   
   # merge catchment df with simulated elementary school df
   sim_school_df <- data.frame(catchID = school_to_catchment_map, schoolID = school_id, 
                               schoolPop = school_pop) |>
     merge(df[,c("catchID", "xStart", "xEnd", "yStart", "yEnd")], 
           by="catchID", all.y = TRUE)
   
   return(sim_school_df)
   
 }
 
 
 
 # Simulate households with children
 subpop_children <- function(df, n = 5){
   
   
   # n is multiplier for extra population 
   total <- sum(df$schoolPop)*n 
   
   
   # random uniform distributions for parent types and children categories
   unif_parent_type <- runif(total)
   unif_child_num <- runif(total)
   unif_childAge1 <- runif(total)
   unif_childAge2 <- runif(total)
   unif_childAge3 <- runif(total)
   
   con <- getOption("usr_con", stdin())
   
   
   # user input for proportions
   cat("Please enter proportion of parents as a couple: ")
   prop_parent_couple <- scan(con, n = 1, what = double())
   
   
   # parent type: 2 = coupled parent, 1 = lone parent
   parent_type <- ifelse(unif_parent_type <= prop_parent_couple, 2, 1)
   
   
   # user input for proportions
   cat("Please enter proportion of coupled parents with 1, 2, 3+ children separated by space:")
   
   prop_children_couple <- scan(con, n = 3, what = double())
   
   cat("Please enter proportion of single parents with 1, 2, 3+ children separated by space:")
   
   prop_children_lone <- scan(con, n = 3, what = double())
   
   
   # simulating number of children based on parent type
   numChildren <- ifelse(parent_type == 2, # number of children if couple with children 
                         ifelse(unif_child_num <= prop_children_couple[1], 1,
                                ifelse(unif_child_num <= sum(prop_children_couple[1:2]),2,3))
                         , ifelse(unif_child_num <= prop_children_lone[1], 1, # number of children if lone-parent with children
                                  ifelse(unif_child_num <= sum(prop_children_lone[1:2]), 2, 3)))
   
   
   # user input for proportions
   cat("Please enter proportion of children that are of elementary school age: ")
   prop_elem_age <- scan(con, n = 1, what = double())
   
   
   # Simulating whether child is of elementary school age
   child1_elemAge <- ifelse(unif_childAge1 <= prop_elem_age,1,0) 
   child2_elemAge <- ifelse(unif_childAge2 <= prop_elem_age,1,0)
   child3_elemAge <- ifelse(unif_childAge3 <= prop_elem_age,1,0)
   
   
   # total number of children in each household
   num_elemChild <- ifelse(numChildren == 3, child1_elemAge + child2_elemAge + child3_elemAge
                           , ifelse(numChildren == 2, child1_elemAge + child2_elemAge, child1_elemAge))
   
   household_W_children <- data.frame(houseID = seq.int(length(parent_type))
                                      , num_parent = parent_type
                                      , num_child = numChildren
                                      , num_elem_child = num_elemChild
                                      , schoolID = rep(0, length(parent_type)))
   
   # Assign elementary aged children to elementary schools 
   #     such that the school population is met
   start <- 0
   stop <-0
   school_assignment <- c()
   
   for(i in 1:nrow(df)){
     
     start <- start + stop
     
     temp_house <- household_W_children[(start+1):nrow(household_W_children),] #households remaining to be assigned
     
     cumul_sum_students <- cumsum(temp_house$num_elem_child) #cumulative sum of elementary aged children yet to be assigned
     
     stop <- which(cumul_sum_students >= df$schoolPop[i])[1] # find the row in which the school population is satisfied
     
     if (is.na(stop)){
       stop <- 0
     } 
     
     school_assignment <- c(school_assignment, rep(df$schoolID[i], stop)) # assign all those households to that school
   }
   
   household_W_children <- household_W_children[1:length(school_assignment),] #keep all assigned households.
   
   household_W_children$schoolID <- school_assignment 
   
   
   # check to see if simulated population is similar
   #print(cbind(aggregate(household_W_children$num_elem_child ~ household_W_children$schoolID, FUN="sum"), df$schoolPop))
   
   
   # update school populations to include the possible extra 1-2 children
   df$schoolPop <- aggregate(household_W_children$num_elem_child ~ household_W_children$schoolID, FUN="sum")[,2]
   
   # include catchment area information into household data frame
   household_W_children <- merge(household_W_children, df, by = "schoolID")
   household_W_children$num_people <- household_W_children$num_child + household_W_children$num_parent
   
   return(household_W_children)
 }
 
 
 #### Simulate Sub-Population 2: households without children ####
 
 # Calculate the household size proportions for households without children.  
 #     Based on the overall household size proportions from the census, 
 #     the proportion of households with children, and the size distribution 
 #     of households with children.
 
 subpop_noChildren <- function(df, df2){
   
   
   # user input for proportion 
   cat("Please enter proportion of households with 1, 2, 3, 4, 5+ members separted by space: ")
   
   prop_household_size <- scan(n = 5, what = double())
   
   cat("Please enter proportion of households with children:  ")
   
   prop_household_Children <- scan(n = 1, what = double())
   
   prop_household_size_nochildren <- c(prop_household_size[1]/(1-prop_household_Children)) # proportion households of size 1
   
   
   # calculate proportion of households of sizes 2-5
   for(i in 1:4){ 
     y <- (prop_household_size[i+1]-prop_household_Children*table(df$num_people)[i]/nrow(df))/(1-prop_household_Children)
     prop_household_size_nochildren <- c(prop_household_size_nochildren, y)
   }
   
   
   #warning message
   if(round(sum(prop_household_size_nochildren)) != 1){
     warning("Sum of proportions of household without children do not equal 1")
   }
   
   
   # Calculate the number of households without children per catchment area.
   # Based on the proportion of households with children.
   catchment_household_count <- data.frame(table(df$catchID))
   
   names(catchment_household_count) <- c("catchID", "num_household_with_children")
   
   catchment_household_count$total_households <- round(catchment_household_count$num_household_with_children/prop_household_Children)
   
   catchment_household_count$num_household_noChildren <- catchment_household_count$total_households - catchment_household_count$num_household_with_children
   
   
   #total number of households with no children
   total <- sum(catchment_household_count$num_household_noChildren)
   
   
   # simulating household size  
   unif_household_size <- runif(total)
   
   household_size <- ifelse(unif_household_size <= prop_household_size_nochildren[1], 1,  
                            ifelse(unif_household_size <= sum(prop_household_size_nochildren[1:2]), 2,
                                   ifelse(unif_household_size <= sum(prop_household_size_nochildren[1:3]), 3,
                                          ifelse(unif_household_size <= sum(prop_household_size_nochildren[1:4]), 4, 5))))
   
   house_no_children <- data.frame(houseID = nrow(df) + seq.int(total),
                                   num_people = household_size,
                                   catchID = rep(0, total))
   
   # Assign households without children to catchment areas
   stop <- 0
   for(i in 1:nrow(catchment_household_count)){
     start <- stop + 1
     stop <- cumsum(catchment_household_count$num_household_noChildren)[i]
     house_no_children$catchID[start:stop] <- catchment_household_count$catchID[i]
   }
   
   house_no_children <- merge(house_no_children, df2[,-2], by="catchID")
   
   return(house_no_children)
   
 }
 
 
 #simulate total households and individuals data
 
 simulate_households <- function(children_df, noChildren_df){
   
   
   
   noChildren_df$schoolID <- 0
   noChildren_df$num_elem_child <- 0
   
   households1 <- children_df[,c("houseID", "catchID", "schoolID", "num_people", "num_elem_child", "xStart", "xEnd", "yStart", "yEnd")]
   households2 <- noChildren_df[,c("houseID", "catchID", "schoolID", "num_people", "num_elem_child", "xStart", "xEnd", "yStart", "yEnd")]
   
   # combining households with and without children
   households <- rbind(households1, households2)
   
   # Generate house locations within catchment areas
   households$loc.x <- runif(nrow(households), households$xStart, households$xEnd)
   households$loc.y <- runif(nrow(households), households$yStart, households$yEnd)
   
   # expanding rows for each individual
   individuals <- households[rep(row.names(households), households$num_people),]
   
   rownames(individuals) <- c() #remove row names
   individuals$individualID <- seq.int(nrow(individuals))
   
   # Create elementary school child indicator
   individuals$elem_child_ind <- 0 
   for(i in unique(individuals$houseID)){
     num_elem <- individuals[which(individuals$houseID == i), "num_elem_child"][1]
     if(num_elem > 0){
       individuals[which(individuals$houseID == i),][1:num_elem, "elem_child_ind"] <- 1
     }
   }
   
   # TEST: make sure the number of elem.child.ind is equal to the number of elementary school children
   if(sum(individuals$elem_child_ind) != sum(households$num_elem_child)){
     warning("number of elementary school children do not match")
   }
   
   return(list(household_sim = households, individual_sim = individuals))
   
   
 }
 
 
 
 



 test_that("interactive", {
   
   
   # set up interactive answers
   f <- file()
   lines <- c(0.7668901,0.3634045, 0.4329440, 0.2036515,0.5857832, 0.3071523, 0.1070645,0.4976825)
   ans <- paste(lines, collapse = "\n")
   write(ans, f)

   options("usr_con" = f) # set connection option
   
   catch_df <- catchment_sim(16, 4.313320, 3.026894, 20)
   
   #simulate elementary schools for each area
   elementary_df <- elementary_pop(catch_df, 5.27426341, 0.01427793)
   
   # simulate household with children and assign them to elementary school
   
   result <- subpop_children(elementary_df)
   
   mean_children1 <- round(mean(aggregate(result$num_elem_child ~ result$schoolID, FUN="sum")[,2])) 
   mean_children2 <- round(mean(elementary_df$schoolPop))


   close(f) # close the file
   
   options("usr_con" = stdin()) # reset connection option

   # tests
   expect_equal(mean_children1, mean_children1)
 })