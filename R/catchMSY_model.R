#' Age-structured Catch-MSY model
#' @description An age-structured assessment model paramterized using MSY and FMSY  
#' as the leading parameters, and the instantaneous natural mortality  rate. This function generates biomass estimates, estimates of fishing mortality rates, and stock depletion.  It also returns statistical criterion depending on the available data, and non-statistical criterion based on satisfying user specified constraints.
#' 
#' @details
#' The age-structured model is conditioned on the historical catch data 
#' (in weight).  The catch equation assumes that both
#' fishing mortality and natural mortality are occuring simultaneously, and the
#' instantaneous fishing mortality rate is found by solving the Baranov catch 
#' equation \eqn{C =  F/Z*(1-exp(-Z))*B}, for \code{F}. 
#'
#' @param sID Stock ID object
#' @param nlSearch Boolean flag to turn off non-statistical criterion for 
#' non-linear search. Set to TRUE when using non-linear search routines.
#' @export
catchMSYModel <- function(sID,nlSearch=FALSE)
{
	with(sID,{

		# calcAgeSchedules
		nage <- max(age)
		la <- linf*(1.0-exp(-vbk*(age-to)))
		la.sd <- la.cv*la
		wa <- a*la^b
		ma <- plogis(age,ah,gh)
		fa <- wa*ma #fecundity is assumed to be proportional to mature body weight.
		va <- 1.0/(1.0+(exp(-log(19)*((age-sel50)/(sel95-sel50)))));
		#todo  add option for survey selectivy (CIE review request.)
		lx <- exp(-m*(age-min(age)))
		lx[max(age)] <- lx[max(age)]/(1.0-exp(-m))
		phie <- sum(lx*fa)

		# Ford-Brody Growth parameter transformation
		rho   <- exp(-vbk)
		alpha <- winf*(1-rho)
		wk    <- wa[nage-1]
		s     <- exp(-m)
		wbar  <- -(s*alpha+wk*(1-s))/(-1+s*rho)
		# print(wbar)

		# calcBoSteepness
		lz	<- vector("numeric",length=length(age))
		za  <- m + fmsy*va
		sa  <- exp(-za)
		oa  <- (1-sa)
		qa  <- va*oa/za
		t2  <- wa*va^2/za
		t3  <- exp(-za)-oa/za
		lz[1]    <- 1.0
		dlz.df	 <- 0.0
		dphie.df <- 0.0
		dphiq.df <- t2[1]*t3[1]
		for(i in age)
		{
			if(i > min(age))
			{
				lz[i]  <- lz[i-1] * sa[i-1]
				dlz.df <- dlz.df  * sa[i-1] - lz[i-1]*va[i-1]*sa[i-1]
				if(i==max(age))
				{
					lz[i]  <- lz[i]/oa[i]
					dlz.df <- dlz.df/sa[i] - lz[i-1]*sa[i-1]*va[i]*sa[i]/oa[i]^2 
				}
			}
			dphie.df <- dphie.df + fa[i]*dlz.df
			dphiq.df <- dphiq.df + wa[i]*qa[i]*dlz.df + lz[i]*t2[i]*t3[i]
		}
		phif  <- sum(lz*fa)
		phiq  <- sum(lz*qa*wa)
		reck  <- phie/phif - (fmsy*phiq*phie/phif^2*dphie.df) / (phiq+fmsy*dphiq.df)
		steep <- reck/(4+reck)

		re 	   <- msy / (fmsy*phiq)
		ro     <- re*(reck-1.0)/(reck-phie/phif)
		bo     <- ro * phie
		so     <- reck/phie
		beta   <- (reck-1.0)/bo
		spr    <- phif/phie
		dre.df <- ro/(reck-1.0)*phie/phif^2*dphie.df

		# runAgeStructuredModel
		names(data) <- tolower(names(data))
		year <- data$year
		chat <- data$catch
		# cpue <- data$index
		nyr  <- length(year)
		

		N    <- C <- matrix(nrow=length(year),ncol=length(age))
		N[1,]<- ro*lx
		ft   <- vector("numeric",length=length(year))
		bpls <- vector("numeric",length=length(year))
		apo  <- age[-min(age)]
		amo  <- age[-max(age)]
		
		for (i in 1:nyr) 
		{
			ft[i]	   <- getFt(chat[i],m,va,wa,N[i,])
			st         <- exp(-m-ft[i]*va)
			ssb        <- sum(N[i,]*fa)
			# spls       <- exp(-m-ft[i]*va[nage])
			# bpls[i]    <- spls*(alpha*N[i,nage]+rho*)
			if(i < nyr)
			{
				N[i+1,1]   <- so*ssb/(1+beta*ssb)
				N[i+1,apo] <- N[i,amo] * st[amo]
				N[i+1,nage]<- N[i+1,nage]+N[i,nage] * st[nage]
			}
			C[i,] = N[i,] * (1 - st) * (ft[i] * va) / (m + ft[i] * va)
		}
		
		ct  <- rowSums(C)
		bt  <- as.vector(N %*% (va*wa))
		sbt <- as.vector(N %*% fa)
		dt  <- sbt/bo
		depletion <- sbt[nyr]/bo


		
		#----------------------------------------------#
		# NON-STATISTICAL CRITERION                    #
		#----------------------------------------------#
		code <- 0
		if(!nlSearch){
			# check for extinction
			if( any(is.na(sbt)) ) { code <- 1 }

			# check for infinite biomass
			if( any(is.infinite(sbt)) )	  { code <- 2 }
			
			# check for lower bound tolerance
			if(!is.na(depletion) && depletion <= lb.depletion) { code <- 3 }

			# check for upper bound depletion tolerance
			if(!is.na(depletion) && depletion >= ub.depletion) { code <- 4 }

			# check bounds for ft.
			if( max(ft,na.rm=TRUE) > 5.0 
			   || any(is.infinite(ft)) 
			   || min(ft,na.rm=TRUE) < 0 ) { code <- 5}

			# check estimates of steepness
			if( reck <= 1.0 ) { code <- 6 }	
		}

		#----------------------------------------------#
		# STATISTICAL CRITERION                        #
		#----------------------------------------------#
		nll <- rep(0,length=4) ## fit to index, biomass, length comp, mean length
		Q   <- 	Qp <- ML <- LF <- NULL
		# Must first pass the non-statistical criterion.
		if( code == 0 ){
			# Relative abundance (trend info)
			# If code==0, and abundance data exists, compute nll
			if(with(sID$data,exists("index"))){
				if( any(!is.na(data$index)) ) {
					 ii <- which(!is.na(data$index))
					.it <- data$index[ii]
					.se <- data$index.lse[ii]
					if(length(.it)>1){
						.zt    <- log(.it) - log(bt[ii])
						.zbar  <- mean(.zt)
						nll[1] <- -1.0*sum(dnorm(.zt,.zbar,.se,log=TRUE))
					}
					else{
						cat("There is insufficient data to fit to trends.")
					}
				}
			}

			# Absolute biomass index.
			if(with(sID$data,exists("biomass"))){
				if( any(!is.na(data$biomass)) ) {
					ii     <- which(!is.na(data$biomass))
					.it    <- log(data$biomass[ii])
					.bt    <- log(bt[ii])
					.se    <- data$biomass.lse[ii]
					nll[2] <- -1.0*sum(dnorm(.it,.bt,.se,log=TRUE))
				}
			}

			#
			# Size comp likelihood here
			bw <- 1.0 #bin width = 1 cm
			A  <- max(age)
			if(all(grepl("lc.", colnames(data))==FALSE)){
				l1 <- floor(la[1]-3*la.sd[1])
				l2 <- ceiling(la[A]+3*la.sd[A])
				bin  <- seq(1,l2+bw,by=bw)
			}
			if(any(grepl("lc.", colnames(data)))){
				bin <- as.numeric((sapply(1:length(colnames(data)[which(grepl("lc.", colnames(data)))]), function(x) strsplit(colnames(data[which(grepl("lc.", colnames(data)))]), ".", fixed=TRUE)[[x]][2])))
			}
			bw <- diff(bin[1:2])
			ALK<- sapply(bin+0.5*bw,pnorm,mean=la,sd=la.sd)-sapply(bin-0.5*bw,pnorm,mean=la,sd=la.sd)
			
			falk <- function(ii)
			{
				iiQ  <- (N[ii,]*va) %*% ALK	
				return(iiQ)	
			}
			ii <- which(!is.na(data$catch))
			Q  <- sapply(ii,falk) ## vulnerable abundance at length in each year
			ML <- sapply(ii, function(x) sum(Q[,x]*bin)/sum(Q[,x]))
			Qp <- sapply(ii, function(x) Q[,x]/sum(Q[,x]))
			LF <- sapply(ii, function(x) rmultinom(n=1, size=sqrt(1000), prob=Qp[,x]))
				rownames(Q) <- rownames(Qp) <- rownames(LF) <- paste0("lc.", bin)

			if(any(grepl("lc.", colnames(data)))){
				lc <- data[,grep("lc.", colnames(data))]
				il <- which(is.na(rowSums(lc))==FALSE)
				.qobs <- lc[il,]
				.qexp <- t(Qp[,il])
				nll[3] <- -1.0*sum(sapply(il, function(y) dmultinom(x=.qobs[y,], prob=.qexp[y,], log=TRUE)))
			}

			# Mealn length likelihood
			if(with(sID$data,exists("meanlength"))){
				if( any(!is.na(data$meanlength)) ) {
					ii     <- which(!is.na(data$meanlength))
					.mlobs    <- log(data$meanlength[ii])
					.mlexp    <- log(ML[ii])
					.se    <- 1
					nll[4] <- -1.0*sum(dnorm(.mlobs,.mlexp,.se,log=TRUE))
				}
			}
			# matplot((Q),type="l")




		}

		# -------------------------------------------- #
		# PRIORS                                       #
		# -------------------------------------------- #
		pvec <- rep(NA,length=3)
		# prior for parameters
		.x    <- c(m,fmsy,msy)
		pvec <- rep(0,length=3)
		for(.i in 1:3)
		{
			.fn <- paste0("d",dfPriorInfo$dist[.i])
			.p1 <- dfPriorInfo$par1[.i]
			.p2 <- dfPriorInfo$par2[.i]
			.p3 <- dfPriorInfo$log[.i]
			pvec[.i] <- -1.0*do.call(.fn,list(.x[.i],.p1,.p2,.p3))
		}
		
		# cat(m,"\t",fmsy,"\t",msy,"\t",sum(nll)+sum(pvec),"\n")

		out <- list(code=code,
		            bo = bo, h=steep,
		            wa = wa, 
		            reck = reck,spr = spr,
		            nll=sum(nll,na.rm=TRUE),
		            prior=sum(pvec,na.rm=TRUE),
		            dt=dt,bt=bt,sbt=sbt,ft=ft,Q=Q,Qp=Qp,ML=ML,LF=LF)
		return(out)
	})
}