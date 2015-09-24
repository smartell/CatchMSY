## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
	library(catchMSY)
	data(NamibianHake)
	names(hake)
	head(hake$data)

## ------------------------------------------------------------------------
	hake <- runModel(hake)
	print(hake)

## ------------------------------------------------------------------------
library(catchMSY)
library(ggplot2)


# load the hake sID object
data(NamibianHake)

# run the age structured model.
hake <- runModel(hake)

# plot the biomass
qplot(hake$year,hake$bt,geom="line")



