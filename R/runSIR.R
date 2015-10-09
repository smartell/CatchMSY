#' Run Importance Sampling
#' @param sID is the stock object
#' @param n number of samples
#' @param nc number of cores
#' @export
#'
runSIR <- function(sID,n=100,nc=1)
{	

	with(sID,{
		.phi   <- sqrt(diag(V)+mu**2)
		.logMu <- log(mu**2/.phi)
		.logSE <- sqrt(log(.phi**2/mu**2))
		.coVar <- R * (.logSE %o% .logSE)
		T <- rmvt(n,sigma=.coVar,df=30,delta=.logMu)
		colnames(T) <- paste0("log.",names(mu))

		# shared memory parallelism
		doMC::registerDoMC(nc)

		.results <- foreach(i = 1:n) %dopar% {
			sID$m    = exp(T[i,1])
			sID$fmsy = exp(T[i,2])
			sID$msy  = exp(T[i,3])
			
			runModel(sID)
		}


		wtC    <- plyr::ldply(.results,function(x){c("log.wts"=x[['log.wt']],"code"=x[['ell']])})
		wts    <- exp(wtC$log.wt)
		idx    <- sample(1:n,n,replace=TRUE,prob=wts)
		sID$ps <- data.frame(T[idx,],wtC[idx,])

		class(sID) <- c(class(sID),"sir")
		return(sID)
	})
}

#' Interval Sample
#' Draw from the dfPriorInfo
#' @export
IS <- function(sID,n=100,nc=1)
{

	with(sID,{
		S <- NULL
		for(i in 1:3)
		{
			.fn <- paste0("r",dfPriorInfo$dist[i])
			.p1 <- dfPriorInfo$par1[i]
			.p2 <- dfPriorInfo$par2[i]
			
			.xx <-  do.call(.fn,list(n,.p1,.p2))
			S   <- cbind(S,.xx)
		}
		
		#shared memory parallelism
		doMC::registerDoMC(nc)

		.results <- foreach(ip=1:n) %dopar%{
			sID$m    = (S[ip,1])
			sID$fmsy = (S[ip,2])
			sID$msy  = (S[ip,3])
			
			runModel(sID)
		}

		codes  <- plyr::ldply(.results,function(x){c("code"=x[['code']])})
		# wtC    <- plyr::ldply(.results,function(x){
					# c("log.wts"=x[['log.wt']],
					  # "code"=x[['code']],
					  # "depletion"=x[['depletion']])})

		return(.results)
	})
}
