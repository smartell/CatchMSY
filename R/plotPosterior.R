#' Plot posterior samples using ggpairs


plotPS <- function(sID)
{
	with(sID,{
		if(is.null(sID$ps))
		{
			cat("No Posterior Samples. Run SIR algorithm first\n")
			break;
		}

		wts <- exp(ps$log.wts)
		n   <- length(wts)
		idx <- sample(1:n,n,replace=TRUE,prob=wts)
		p <- ggpairs(ps[idx,],columns=1:3)
		print(p)
		# browser()
	})

}