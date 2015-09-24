#' Run Sample Imporance Resample routine.
#' @param sID is the stock object
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
		registerDoMC(nc)

		.results <- foreach(i = 1:n) %dopar% {
			sID$m    = exp(T[i,1])
			sID$fmsy = exp(T[i,2])
			sID$msy  = exp(T[i,3])
			
			.runModel(sID)
		}


		wtC    <- ldply(.results,function(x){c("log.wts"=x[['log.wt']],"code"=x[['ell']])})
		wts    <- exp(wtC$log.wt)
		idx    <- sample(1:n,n,replace=TRUE,prob=wts)
		sID$ps <- data.frame(T[idx,],wtC[idx,])

		class(sID) <- c(class(sID),"sir")
		return(sID)
	})
}
