#' Calculate negative loglikelihood for relative abundance index.
#' @details  Computes the negative loglikelihood of the relative abundance index and adds
#'  nll.index to the sID object.
#' @return NULL
calcNllIndex <- function(sID)
{
	within(sID,{
		# likelihood for index data.
		.index    <- data$index
		.lse      <- data$index.lse
		.zt       <- log(.index) - log(bt)
		.zbar     <- mean(.zt)
		epsilon   <- (.zt - .zbar)/.lse
		it_hat    <- exp(.zbar) * bt
		nll.relIndex <- -1.0 * sum(dnorm(.zt,.zbar,.lse,log=TRUE))
	})
}

#' Calculate negative loglikelihood for absolute abundance.
#' 
#' 
calcNLLAbsoluteBiomass <- function(sID)
{
  within(sID,{
    # likelihood for absolutee abundance data.
    .absBt    <- data$biomass
    .lse      <- data$biomass.lse
    .zt       <- log(.absBt) - log(bt)
    nll.absIndex <- -1.0 * sum(dnorm(.zt,0,.lse,log=TRUE))
  })
}

#'Prior density for each parameter
calcPrior <- function(sID)
{
	within(sID,{
		# prior for parameters
		.x    <- c(m,fmsy,msy)
		pvec <- rep(0,length=3)
		for(.i in 1:3)
		{
			.fn <- paste0("d",dfPriorInfo$dist[.i])
			.p1 <- dfPriorInfo$par1[.i]
			.p2 <- dfPriorInfo$par2[.i]
			.p3 <- dfPriorInfo$log[.i]
			pvec[.i] <- -1.0 * do.call(.fn,list(.x[.i],.p1,.p2,.p3))
		}
	})
}


#' Compute the objective function.
#' @details Computes the objective function for Importance Sampling or non-linear parameter estimation.
#' 
#' @return sID object
calcObjectiveFunc <- function(sID)
{
	sID <- calcNllIndex(sID)
	sID <- calcPrior(sID)

	# Bernoulli likelihood for stock extinction.
	within(sID,{
		# code is the object for pass (0), fail (!0)
		p.vec <- rep(0,length=1)

		# CONVERGENCE CODES (code)
		code   <- 0
		# check for extinction
		if(min(sbt,na.rm=TRUE) <= 0 ) { code <- 1 }

		# check for infinite biomass
		if(any(is.infinite(sbt)))	  { code <- 2 }
		
		# check for lower bound tolerance
		if(depletion <= lb.depletion) { code <- 3 }

		# check for upper bound depletion tolerance
		if(depletion >= ub.depletion) { code <- 4 }


		# Importance function:
		# negative log of multivariate-t dist.
		.x  <- c(m,fmsy,msy)
		.mu <- sID$mu  # prior mean
		.V  <- sID$V   # Variance Covariance 
		p.theta  <- -1.0 * mvtnorm::dmvt(.x,delta=.mu,sigma=.V,df=30,log=TRUE)

		# prior for depletion boundary constraints
		p.vec[1] <- -1.0 * dunif(depletion,lb.depletion,ub.depletion,log=TRUE)
		

		nll    <- nll.relIndex + sum(pvec)
		log.wt <- -nll + p.theta
		# log.wt    <- -(nll.index+sum(p.vec)) + p.theta 
		if(is.na(log.wt)) log.wt <- -Inf
		return(sID)
	})

}