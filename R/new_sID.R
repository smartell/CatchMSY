#' Generate a new stock ID object
#' 
#' @param id Name of Stock
#' @param dfile Name of file containing time series data
#' @return A template for a stock class object.
#' @details This function returns a list object containing all the necessary
#'          information to run the catchMSY routines.  
#' @examples 
#' #myStock <- new_sID()
#' #names(myStock)
#'
#' @export
new_sID <- function(id  = "Stock Label",
                    age = 1:10,
                    linf = 100,
                    winf = 5.0,
                    vbk  = 0.2,
                    to   = 0.0,
                    a    = 5e-6,
                    b    = 3.0,
                    mat_age_50  = 2.0,
                    mat_age_95  = 3.0,
                    dfile="")
{
	S     <- list()
	S$id  <- id
	S$age <- age

	# growth parameters
	S$linf <- linf
	S$winf <- winf
	S$vbk  <- vbk
	S$to   <- to
	S$a    <- a
	S$b    <- b

	# maturity parameters
	S$ah   <- mat_age_50
	S$gh   <- mat_age_95

	# selectivity parameters
	S$sel50 <- 2.0
	S$sel95 <- 5.0

	# population parameters
	S$m    <- 0.15
	S$fmsy <- 0.25
	S$msy  <- 265.

	# data frame for parameter priors
	S$dfPriorInfo <- data.frame(id=1:3,
      dist=c("lnorm","unif","norm"),
      par1=c(log(0.2),0,S$msy),
      par2=c(0.01,1.0,0.2*S$msy),
      log = TRUE,
      stringAsFactors=FALSE)


	# vector of parameters for prior samples
	S$mu <- c(m=S$m,fmsy=S$fmsy,msy=S$msy)
	S$sd <- S$mu * c(0.01,0.1,0.3)
	S$R  <- diag(1,nrow=3,ncol=3)
	S$R[lower.tri(S$R)] <- c(+0.50,-0.10,0.00)
	S$R[upper.tri(S$R)] <- S$R[lower.tri(S$R)]
	S$V  <- S$R*(S$sd%o%S$sd)

	# constraints
	S$lb.depletion <- 0.01
	S$ub.depletion <- 0.80

	# DATA FRAME TEMPLATE FOR TIME SERIES DATA	
	df <- data.frame("year"=1,
	                 "catch"=1,
	                 "index"=1,
                    "index.lse"=0.1,
                    "biomass"=NA,
                    "biomass.lse"=NA,
                    "avgSize"=NA
                   )
	
	
	if(file.exists(dfile)){
		S$data <- read.table(dfile,header=TRUE)

		# bn <- sub("\\.[[:alnum:]]+$", "",dfile)
		# save(S,file=paste0(bn,".rda"))	
	} else {
		S$data <- df
	}


	return(S)
}
