#' Envoke a non-linear search routine to estimate model parameters.
#' @description Nonlinear search routine for estimating model parameters given data.
#' @details The solver function relies on 'optim' to conduct a non-linear search
#' for estimating model parameters (M, FMSY, MSY).
#' @return sID object
#' @export
solver <- function(sID, selex=FALSE)
{
	fn <- function(theta)
	{
		sID$m    <- exp(theta[1])
		sID$fmsy <- exp(theta[2])
		sID$msy  <- exp(theta[3])
		if(selex==TRUE & sID$smodel=="logistic") sID$sel1 <- exp(theta[4])
		if(selex==TRUE & sID$smodel=="dome") warning("Not programmed to estimate dome-shaped selectivity parameters")
		# nll      <- runModel(sID)$nll	
		rm      <- catchMSYModel(sID,nlSearch=TRUE)	
		# print(rm$nll)
		return(rm$nll+rm$prior)
	}

	within(sID,{
		.theta <- log(mu)
		.fit   <- optim(.theta,fn,method="L-BFGS-B",lower=rep(0,3),upper=c(0.8,5.0,1000),hessian=TRUE)
		.H     <- .fit$hessian
		V      <- solve(.H)
		mu     <- .fit$par
		sd     <- sqrt(diag(V))
		R      <- V / (sd %o% sd)
		m      <- mu[1]
		fmsy   <- mu[2]
		msy    <- mu[3]
		sID    <- catchMSYModel(sID)
		return(sID)
	})
}