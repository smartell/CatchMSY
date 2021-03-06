#' Generate prior samples for importance sampling.
#' @param sID species ID list
#' @param n number of samples to generate
#' @param selex default FALSE - only samples from parameters m, fmsy, and msy. Set to TRUE to also sample sel50
#' @description Draw random samples from specified parameter ranges in the 
#' stock ID object.
#' @export
sample.sid <- function(sID,n=100,selex=FALSE)
{
	within(sID,{
		S <- NULL
		if(selex==FALSE) vec <- 1:3
		if(selex==TRUE) vec <- 1:4
		for(.i in vec)
		{
			.fn <- paste0("r",dfPriorInfo$dist[.i])
			.p1 <- dfPriorInfo$par1[.i]
			.p2 <- dfPriorInfo$par2[.i]
			
			## generates n number of draws from .fn distribution given parameters .p1 and .p2
			## for each parameter (m, fmsy, msy)
			.xx <-  do.call(.fn,list(n,.p1,.p2))

			S   <- cbind(S,.xx)
		}
		if(selex==FALSE) colnames(S) <- c("m","fmsy","msy")
		if(selex==TRUE) colnames(S) <- c("m","fmsy","msy","sel50")
	})
}


#' Importance Sampling
#' @param sID Stock ID object
#' Set to TRUE when including the prior on sel1 (sel50).
#' @param nc  Number of cores for parrallel processing.
#' @param selex Boolean flag to turn off search across sel1 (sel50) parameter. 
#' @export
sir.sid <- function(sID,ncores=1,selex=FALSE)
{
	with(sID,{
		n    <- dim(S)[1]
		# cmsy <- NULL
		# for(i in 1:n)
		# {
		# 	sID$m    <- S[i,1]
		# 	sID$fmsy <- S[i,2]
		# 	sID$msy  <- S[i,3]
		# 	cmsy <- c(cmsy,catchMSYModel(sID))
		# }

		#shared memory parallelism
		# doMC::registerDoMC(nc)
		### runs catchMSY_model -- if different checks are TRUE, removes this parameter combination from the possibilities
		if(ncores > 1)
		{
			registerDoParallel(cores=ncores)

			.results <- foreach(i = 1:n, .export=c("sID","selex")) %dopar% {
				sID$m    <- S[i,1]
				sID$fmsy <- S[i,2]
				sID$msy  <- S[i,3]
				if(selex==TRUE & sID$smodel=="logistic"){
					sID$sel1 <- S[i,4]
					sID$sel2 <- S[i,4]+1
				}
				if(selex==TRUE & sID$smodel=="dome") warning("Not programmed to estimate dome-shaped selectivity parameters")
				return(catchMSYModel(sID))
			}
			cmsy  <- .results			
		}
		else {
			fn <- function(s){
				sID$m    <- s[1]
				sID$fmsy <- s[2]
				sID$msy  <- s[3]
				if(selex==TRUE & sID$smodel=="logistic"){
					sID$sel1 <- s[4]
					sID$sel2 <- s[4]+1
				}
				if(selex==TRUE & sID$smodel=="dome") warning("Not programmed to estimate dome-shaped selectivity parameters")
				return(catchMSYModel(sID))
			}
			cmsy  <- apply(S,1,fn)		
		}

		sID$code   <- plyr::ldply(cmsy,function(x){c("code"=x[['code']])})
		sID$bo     <- plyr::ldply(cmsy,function(x){c("bo"=x[['bo']])})
		sID$h      <- plyr::ldply(cmsy,function(x){c("h"=x[['h']])})
		sID$nll    <- plyr::ldply(cmsy,function(x){c("nll"=x[['nll']])})
		sID$prior  <- plyr::ldply(cmsy,function(x){c("prior"=x[['prior']])})
		sID$ps.bt  <- plyr::ldply(cmsy,function(x){c("bt"=x[['bt']])})
		sID$ps.dt  <- plyr::ldply(cmsy,function(x){c("dt"=x[['dt']])})
		sID$ps.sbt <- plyr::ldply(cmsy,function(x){c("sbt"=x[['sbt']])})
		sID$ps.ft  <- plyr::ldply(cmsy,function(x){c("ft"=x[['ft']])}) 
		sID$wts    <- exp(-(sID$nll + sID$prior))
		sID$ML <- plyr::ldply(cmsy, function(x){c("ML"=x[['ML']])})
		sID$LF <- sapply(1:length(cmsy), function(x) cmsy[[x]][['LF']])
		sID$spr_msy <- plyr::ldply(cmsy, function(x){c("spr_msy"=x[['spr_msy']])})
		sID$spr_t <- plyr::ldply(cmsy, function(x){c("spr_t"=x[['spr_t']])})
		sID$biomass_resid <- plyr::ldply(cmsy, function(x){c("biomass_resid"=x[['biomass_resid']])})
		sID$index_resid <- plyr::ldply(cmsy, function(x){c("index_resid"=x[['index_resid']])})
		sID$lc_resid <- sapply(1:length(cmsy), function(x) cmsy[[x]][['lc_resid']])
		sID$ml_resid <- plyr::ldply(cmsy, function(x){c("ml_resid"=x[['ml_resid']])})

		## non-statistical criterion - sample combinations that meet criterion
		sID$idx    <- which(sID$code==0)
		## statistical criterion - sample combinations that nll!=0
		## choose samples with the highest probability - best likelihoods
		ic <- which(sID$nll!=0)
		if( length(ic) > 0 ){
			prb <- sID$wts[ic,1]
			idx_new  <- tryCatch(sample(ic,length(ic),replace=TRUE,prob=prb), error=function(e) NULL)
			if(all(is.null(idx_new))==FALSE) sID$idx <- idx_new
		}
		return(sID)
	})
}



#' Interval Sample
#' Draw from the dfPriorInfo
#' 
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


#' Run Importance Sampling
#' @param sID is the stock object
#' @param n number of samples
#' @param nc number of cores
#'
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


