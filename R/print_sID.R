#' Print sid object

print.sid <- function(object, ...)
{
	h <- object$reck/(object$reck+4)


	cat("sID:\n")
	cat("Plus Group Age \t:",max(object$age),"\n")

	
	cat("\nAssessment Summary\n")
	cat("---------------------------------------\n")
	cat("  Unfished Spawning Biomass\t:",round(object$bo,0)  ,"\n")
	cat("  Steepness of S–R model   \t:",round(h,2)          ,"\n")
	cat("  Maximum Sustainable Yield\t:",round(object$msy,0) ,"\n")
	cat("  Optimal Fishing Rate FMSY\t:",round(object$fmsy,2),"\n")
	cat("  Natural Mortality Rate   \t:",round(object$m,2)   ,"\n")
	cat("  Current depletion level  \t:",round(object$depletion,2)   ,"\n")
	cat("---------------------------------------\n")



	cat("Time Series:\n")
	df <- data.frame(Year = object$year,
	                 Catch = round(object$chat,0),
	                 Biomass = round(object$bt,0),
	                 "Spawn Biomass" = round(object$sbt,0),
	                 "Recruitment" = round(object$N[,1],0),
	                 F   = round(object$ft,2))
	print(rbind(head(df,10)))
}
