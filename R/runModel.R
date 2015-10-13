#' A wrapper for runing the age-structured model.
#' @export
runModel <- function(sID)
{
	sID <- calcAgeSchedules(sID)
	sID <- calcBoSteepness(sID)
	sID <- runAgeStructuredModel(sID)
	sID <- calcObjectiveFunc(sID)

	class(sID) <- c("list","sid")
	return(sID)
}



#' Envoke a non-linear search routine to estimate model parameters.
#' @return sID object
#' @export
solver <- function(sID)
{
	fn <- function(theta)
	{
		sID$m    <- exp(theta[1])
		sID$fmsy <- exp(theta[2])
		sID$msy  <- exp(theta[3])
		# nll      <- runModel(sID)$nll	
		rm      <- catchMSYModel(sID,search=TRUE)	
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
		sID    <- runModel(sID)
		return(sID)
	})
}