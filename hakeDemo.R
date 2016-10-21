# Demo using the Nambibian hake data.
# load the catchMSY library
library(catchMSY)
library(ggplot2)
.THEME <- theme_bw(20)

# Test that the model runs.
## hake built in to the package
catchMSYModel(hake)


## parameter 1 is M -- lognormal prior with mean 0.2 and sd =1
## parameter 2 is Fmsy -- uniform prior between 0 and 1
## parameter 3 is MSY -- default normal prior
# Change to uniform prior for msy
hake$dfPriorInfo$dist[3]<-'unif'
hake$dfPriorInfo$par1[3]<- quantile(hake$data$catch, 0.05)
hake$dfPriorInfo$par2[3]<- quantile(hake$data$catch, 0.975)


# Change selectivity
hake$sel1 <- 4.0 ## age at 50% selectivity
hake$sel2 <- 5.0 ## age at 95% selectivity

# Generate random samples from dfPriorInfo
nsamp <- 500
hake <- sample.sid(sID=hake,n=nsamp)
pairs(hake$S,gap=0, pch=19)

# Now run sir routine
hake <- sir.sid(hake,ncores=1)
pairs(hake$S,gap=0,col=unlist(hake$code+1),pch=19)

# Add Bo and h to samples
ps <- cbind(hake$S,hake$bo,hake$h)
colnames(ps) <- c("M","Fmsy","MSY","Bo","Steepness")
pairs(ps,gap=0,col=unlist(hake$code+1),pch=19)

### narrow down 'chosen' vs 'removed' samples without color specifications
iclr <- c("steelblue","lightgrey")
clr  <- iclr[unlist(sign(hake$code)+1)]
pairs(hake$S,pch=19,gap=0,col=clr)

## zoom in to B0 and steepness
jpd  <- data.frame(Bo=hake$bo,Steepness=hake$h)
colnames(jpd) <- c("Unfished biomass (Bo)","Steepness (h)")
pairs(jpd,gap=0,pch=19)
pairs(jpd,gap=0,pch=19,col=clr)

## scatterplot with marginal densities
df<- with(hake,{
	df <- data.frame(S,code=code,clr='black')
	pdf <- subset(df,code==0)
	ndf <- subset(df,code!=0)
	ndf$clr <- 'red'
	return(rbind(pdf,ndf))
})

p <- ggpairs(df,1:3,mapping=ggplot2::aes(color=clr,alpha=0.2))
p <- ggpairs(df,1:3,upper = list(continuous = "density", combo = "box"))
p <- ggpairs(df,1:3,upper = "blank",mapping=ggplot2::aes(color=clr,alpha=0.2))
print(p + .THEME)

## chosen depletion trajectories from possibilities

dfd <- with(hake,{
	df  <- ps.dt[code==0,]
	colnames(df) = data$year
	df <- cbind(id=1:dim(df)[1],df)
	return(gather(df,key,dt,-id))
})

p <- ggplot(dfd,aes(x=as.double(key),y=dt)) 
p <- p + stat_summary(geom="ribbon",fun.ymin="min",fun.ymax="max",alpha=0.1)
p <- p + stat_summary(geom="ribbon", fun.data="median_hilow",alpha=0.2,fill="blue")
p <- p + stat_summary(geom="ribbon", fun.data="mean_cl_boot",alpha=0.2,fill="red")
p <- p + geom_line(aes(group=id),size=0.1,alpha=0.1)
p <- p + labs(x="Year",y="Depletion") + ylim(0,1)
print(p + .THEME)

## MSY histogram
par(mfrow=c(1,1))
xlim <- c(0, hake$dfPriorInfo$par2[3]*1.2)
ylim <- c(0, nsamp/5)
hist(hake$S[,"msy"], col="gray", lty="blank", xlim=xlim, ylim=ylim, xlab="MSY", ylab="Frequency", main="")
par(new=TRUE)
hist(hake$S[hake$idx,"msy"], col="#AA000050", lty="blank", xlim=xlim, ylim=ylim, xlab="", ylab="", main="")
legend("topright", legend=c("Sampling space", "Catch only"), pch=15, col=c("gray", "#AA000050"), cex=2)
