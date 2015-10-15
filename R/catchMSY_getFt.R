#' Use Newtons method to find instantaneous F
#' @return instantaneous fishing mortality rate F
getFt <- function(ct,m,va,wa,na)
{	#use newtons root finding method to get ft
	ft 	<- ct/(sum(na*exp(-m/2)*wa*va))	
	for(iter in 1:9)
	{
		T1	<- wa*na
		T2	<- exp(-m-ft*va)
		T3	<- (1-T2)
		T4	<- m+ft*va
		c1	<- sum(ft*va*T1*T3/T4)
		c2	<- sum(va*T1*T3/T4 - ft*va^2*T1*T3/T4^2 + ft*va^2*T1*T2/T4)
		ft	<- ft - (c1-ct)/c2	#newton step.
	}
	
	# cat("ft = ",ft,"\n")
	return (ft)
}

