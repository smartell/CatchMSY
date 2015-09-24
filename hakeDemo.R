#hake.R
# load the catchMSY library
library(catchMSY)
library(ggplot2)



# Create a new sID object
new_sID(id="Namibian Hake",dfile="NamibianHake.dat")

# load the hake sID object and assign to hake
data(NamibianHake)



# run the age structured model using defaults
hake <- runModel(hake)


# estimate parameters
hake <- solver(hake)


# plot the biomass
p <- qplot(hake$year,hake$bt,geom="line")
print(p)




