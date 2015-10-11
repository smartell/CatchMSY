catchMSYModel <- function(sID)
{
	with(sID,{

		# calcAgeSchedules
		la <- linf*(1.0-exp(-vbk*(age-to)))
		wa <- a*la^b
		ma <- plogis(age,ah,gh)
		fa <- wa*ma
		va <- 1.0/(1.0+(exp(-log(19)*((age-sel50)/(sel95-sel50)))));
		lx <- exp(-m*(age-min(age)))
		lx[max(age)] <- lx[max(age)]/(1.0-exp(-m))
		phie <- sum(lx*fa)

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
		dt  <- sbt/bo
		depletion <- sbt[nyr]/bo

		# calcObjectiveFunction
		# non-statistical criterion
		code <- 0

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

		out <- list(code=code,dt=dt,bt=bt,sbt=sbt,ft=ft)
		return(out)
	})
}