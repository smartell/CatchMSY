#' Calculate age-schedule information.
#'
#' @details Computes the length-at-age, weight-at-age
#' maturity-at-age,survivorship, selectivity, and SSB per recruit.
#'
#' @return sID object with additional information.
#' @export
calcAgeSchedules <- function(sID)
{
	within(sID,{
	     la <- linf*(1.0-exp(-vbk*(age-to)))
	     wa <- a*la^b
	     ma <- plogis(age,ah,gh)
	     fa <- wa*ma
	     va <- 1.0/(1.0+(exp(-log(19)*((age-sel50)/(sel95-sel50)))));
	     lx <- exp(-m*(age-min(age)))
	     lx[max(age)] <- lx[max(age)]/(1.0-exp(-m))
	     phie <- sum(lx*fa)
		return(sID)
	})
}
