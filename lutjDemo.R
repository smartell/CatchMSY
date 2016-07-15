#lutj.R
#load the library 
#If you need to obtain the catch MSY package use:
devtools::install_github("smartell/CatchMSY",build.vignettes=TRUE)
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
lutjanid$la.cv <- 0.07
lutjanid$a    <- 2.447128e-05 #units are lbs
lutjanid$b    <- 3.154
lutjanid$winf <- lutjanid$a * lutjanid$linf^lutjanid$b

lutjanid$m    <- 1.50 * lutjanid$vbk
lutjanid$ah   <- 1.65 / lutjanid$m
lutjanid$gh   <- 0.10 * lutjanid$ah

# Set default MSY and FMSY values based on data
lutjanid$msy  <- median(lutjanid$data$catch)
lutjanid$fmsy <- 0.6 * lutjanid$m

# Set age-at-entry to the fishery.
lutjanid$sel50 <- 3
lutjanid$sel95 <- 4

# Add option to specify selectivity in terms of length.

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

#|-----------------------------------------------------------------|#
#|	CATCH ONLY METHOD                                              |#
#|-----------------------------------------------------------------|#
	M0      <- lutjanid
	# remove biomass data for the Catch only Method
	M0$data <- M0$data[,1:2]

	# generate samples from parameter samples
	M0      <- sample.sid(M0,.NSAMP)

	# run model with each sample
	M0      <- sir.sid(M0,ncores=.NCORE)

	# Get MSY statistics
	M0$msy.stats <- summary(M0$S[M0$idx,3])


#|------------------------------------------------------------|#
#|	CATCH WITH BIOMASS                                        |#
#|----------------------------------------------------------- |#
	M1      <- lutjanid
	M2      <- lutjanid
	M2$dfPriorInfo$par1[3] = quantile(lutjanid$data$catch,0.05)
	M2$dfPriorInfo$par2[3] = 1.5*quantile(lutjanid$data$catch,0.95)

	# generate samples from parameter samples
	M1      <- sample.sid(M1,.NSAMP)
	M2      <- sample.sid(M2,.NSAMP)

	# run model with each sample
	M1      <- sir.sid(M1,ncores=.NCORE)
	M2      <- sir.sid(M2,ncores=.NCORE)

	# Get MSY statistics
	M1$msy.stats <- summary(M1$S[M1$idx,3])
	M2$msy.stats <- summary(M2$S[M2$idx,3])

#|----------------------------------------------------------- |#
#|	SIMULATION MODEL                                          |#
#|------------------------------------------------------------|#
	S1      <- lutjanid
	R1      <- catchMSYModel(S1)
	ii      <- which(!is.na(S1$data$biomass))
	# xx      <- R1$bt[ii]*exp(rnorm(length(ii),0,S1$data$biomass.lse[ii]))
	# S1$data$biomass[ii] <- xx
	S1$data$biomass[ii] <- rlnorm(length(ii),log(R1$bt[ii]),S1$data$biomass.lse[ii])

	print(S1$msy)

	S1 <- sample.sid(S1,.NSAMP)
	S1 <- sir.sid(S1,ncores=.NCORE)
	S1$msy.stats <- summary(S1$S[S1$idx,3])

#|-------------------------------------------------------|#
#|	CATCH WITH SIZE COMP                                 |#
#|-------------------------------------------------------|#

	## simulate length comp data
	Ssim <- lutjanid
	Ssim$la.cv <- 0.15 ## adjust length CV to simulate messy data
	Rsim <- catchMSYModel(Ssim)

	## add generated length comp data to observed catch data
	M3 <- lutjanid
	M3$data <- cbind(lutjanid$data[,-grep("biomass", colnames(lutjanid$data))], t(Rsim$Qp), "ESS"=rep(1,nrow(lutjanid$data)))

	# generate samples from parameter samples
	M3      <- sample.sid(M3,.NSAMP)

	# run model with each sample
	M3      <- sir.sid(M3,ncores=.NCORE)

	# Get MSY statistics
	M3$msy.stats <- summary(M3$S[M3$idx,3])

#|-------------------------------------------------------|#
#|	CATCH WITH MEAN LENGTH                               |#
#|-------------------------------------------------------|#

	## simulate length comp data
	Ssim <- lutjanid
	Ssim$la.cv <- 0.15 ## adjust length CV to simulate messy data
	Rsim <- catchMSYModel(Ssim)

	## add generated mean length to observed catch data
	M4 <- lutjanid
	M4$data <- cbind(lutjanid$data[,-grep("biomass", colnames(lutjanid$data))], "meanlength"=Rsim$ML)

	# generate samples from parameter samples
	M4      <- sample.sid(M4,.NSAMP)

	# run model with each sample
	M4      <- sir.sid(M4,ncores=.NCORE)

	# Get MSY statistics
	M4$msy.stats <- summary(M4$S[M4$idx,3])


#|-------------------------------------------------|#
#|	GRAPHICS AND SUMMARY STATISTICS                                         |#
#|-------------------------------------------------|#
	plot(M0,.THEME)
	plot(M1,.THEME)

	quartz()
	M1$sel50 <- 3
	M1$sel95 <- 4
	Q <- catchMSYModel(M1)$Q
	matplot(Q,type="l")

	quartz()
	M1$sel50 <- 2
	M1$sel95 <- 3
	Q <- catchMSYModel(M1)$Q
	matplot(Q,type="l")