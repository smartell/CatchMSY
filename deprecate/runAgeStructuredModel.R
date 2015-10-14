#' Run Age Structured Model on sID object.
#' @param sID A stock object.
#' @return A modified \code{sID} containing the output from an age-structured
#'         model.
#' @details
#' The age-structured model is conditioned on the historical catch data 
#' (in weight).  The catch equation assumes that both
#' fishing mortality and natural mortality are occuring simultaneously, and the
#' instantaneous fishing mortality rate is found by solving the Baranov catch 
#' equation \eqn{C =  F/Z*(1-exp(-Z))*B}, for \code{F}. 
#' 

runAgeStructuredModel <- function(sID)
{
	
	within(sID,{
		# Extract data elements.
		names(data) <- tolower(names(data))
		year <- data$year
		chat <- data$catch
		cpue <- data$index
		nyr  <- length(year)
		

		N    <- matrix(nrow=length(year),ncol=length(age))
		N[1,]<- ro*lx
		ft   <- vector("numeric",length=length(year))
		apo  <- age[-min(age)]
		amo  <- age[-max(age)]
		nage <- max(age)
		for (i in 1:nyr) 
		{
			ft[i]	   <- getFt(chat[i],m,va,wa,N[i,])
			st         <- exp(-m-ft[i]*va)
			ssb        <- sum(N[i,]*fa)
			if(i < nyr)
			{
				N[i+1,1]   <- so*ssb/(1+beta*ssb)
				N[i+1,apo] <- N[i,amo] * st[amo]
				N[i+1,nage]<- N[i+1,nage]+N[i,nage] * st[nage]
			}
		}
		
		bt  <- as.vector(N %*% (va*wa))
		sbt <- as.vector(N %*% fa)
		depletion <- sbt[nyr]/bo
		return(sID)
	})
	
}


