#lutj.R
#load the library 
#If you need to obtain the catch MSY package use:
# devtools::install_github("smartell/catchMSY",build.vignettes=TRUE)
library(catchMSY)

.NSAMP <- 5000
.NCORE <- 8
.THEME <- theme_bw(12)


# Create a new stockID
lutjanid     <- new_sID(id="Lutjanidae",dfile="./inst/extdata/lutj.dat")

# Use Beverton Holt factors to establish M,ah,
# Growth parameters taken for Taape (lutjanus kasmira)
# Reference:(Moralis-Nin & Ralston, 1989)
lutjanid$linf <- 34.0
lutjanid$vbk  <- 0.29
lutjanid$a    <- 2.447128e-05 #units are lbs
lutjanid$b    <- 3.154

lutjanid$m    <- 1.50 * lutjanid$vbk
lutjanid$ah   <- 1.65 / lutjanid$m
lutjanid$gh   <- 0.10 * lutjanid$ah

# Set default MSY and FMSY values based on data
lutjanid$msy  <- median(lutjanid$data$catch)
lutjanid$fmsy <- 0.6 * lutjanid$m

# Set age-at-entry to the fishery.
lutjanid$sel50 <- 3
lutjanid$sel95 <- 4

# Set parameter sampling frame
lutjanid$dfPriorInfo$dist[1] = "lnorm"
lutjanid$dfPriorInfo$par1[1] = log(lutjanid$m)
lutjanid$dfPriorInfo$par2[1] = 0.05 * lutjanid$m
lutjanid$dfPriorInfo$dist[2] = "unif"
lutjanid$dfPriorInfo$par1[2] = 0.20 * lutjanid$m
lutjanid$dfPriorInfo$par2[2] = 1.50 * lutjanid$m
lutjanid$dfPriorInfo$dist[3] = "unif"
lutjanid$dfPriorInfo$par1[3] = quantile(lutjanid$data$catch,0.05)
lutjanid$dfPriorInfo$par2[3] = quantile(lutjanid$data$catch,0.95)

#|---------------------------------------------------------------------------|#
#|	CATCH ONLY METHOD  M0                                                    |#
#|---------------------------------------------------------------------------|#
	M0      <- lutjanid
	# remove biomass data for the Catch only Method
	M0$data <- M0$data[,1:2]

	# generate samples from parameter samples
	M0      <- sample.sid(M0,.NSAMP)

	# run model with each sample
	M0      <- sir.sid(M0,ncores=.NCORE)


	# Get MSY statistics
	M0$msy.stats <- summary(M0$S[M0$code==0,3])


#|---------------------------------------------------------------------------|#
#|	CATCH WITH BIOMASS                                                       |#
#|---------------------------------------------------------------------------|#
	M1      <- lutjanid

	# generate samples from parameter samples
	M1      <- sample.sid(M1,.NSAMP)

	# run model with each sample
	M1      <- sir.sid(M1,ncores=.NCORE)

	# Get MSY statistics
	M1$msy.stats <- summary(M1$S[M0$code==0,3])


#|---------------------------------------------------------------------------|#
#|	GRAPHICS AND SUMMARY STATISTICS                                          |#
#|---------------------------------------------------------------------------|#
	plot(M0,.THEME)
	plot(M1,.THEME)

	Q <- catchMSYModel(M1)$Q
	matplot(Q,type="l")
