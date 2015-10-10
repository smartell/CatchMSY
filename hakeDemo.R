#hake.R
# load the catchMSY library
library(catchMSY)
library(tidyr)
library(ggplot2)
library(mvtnorm)
library(foreach)




# 
# TEST CATCH ONLY METHOD USING hake0
# 
load("Hake_0.rda")
hake0 <- sample.sid(hake0)
hake0 <- sir.sid(hake0)
# hake0 <- runModel(hake0)
# plot(hake0)





# # Create a new sID object
# # new_sID(id="Namibian Hake",dfile="NamibianHake.dat")

# # load the hake sID object and assign to hake
# data(NamibianHake)


# # estimate parameters
# hake <- solver(hake)


# # run the age structured model using defaults
# hake <- runModel(hake)

# # Plot the results
# plot(hake)





