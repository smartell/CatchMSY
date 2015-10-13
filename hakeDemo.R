#hake.R
# load the catchMSY library
library(catchMSY)
library(tidyr)
library(ggplot2)
library(GGally)
library(mvtnorm)
library(foreach)

.THEME <- theme_bw(20)



# 
# TEST CATCH ONLY METHOD USING hake0
# 
load("Hake_0.rda")

# Change to uniform prior for msy
hake0$dfPriorInfo$dist[3]<-'unif'
hake0$dfPriorInfo$par1[3]<-100
hake0$dfPriorInfo$par2[3]<-400


hake0 <- sample.sid(hake0,500)
hake0 <- sir.sid(hake0,1)

with(hake0,{
	matplot(t(ps.dt[code==0,]),type="l",ylim=c(0,1))
	matplot(t(ps.bt[code==0,]),type="l")
	matplot(t(ps.ft[code==0,]),type="l",col="red",lty=1)
})

iclr <- c("steelblue","lightgrey")
clr  <- iclr[unlist(sign(hake0$code)+1)]
jpd  <- data.frame(Bo=hake0$bo,Steepness=hake0$h)
colnames(jpd) <- c("Unfished biomass (Bo)","Steepness (h)")
pairs(hake0$S,pch=20,gap=0)
pairs(hake0$S,pch=20,gap=0,col=clr)
pairs(jpd,gap=0,pch=20)
pairs(jpd,gap=0,pch=20,col=clr)



df2 <- with(hake0,{
	df  <- ps.dt[code==0,]
	colnames(df) = data$year
	df <- cbind(id=1:dim(df)[1],df)
	return(gather(df,key,dt,-id))
})




p <- ggplot(df2,aes(x=as.double(key),y=dt)) 
p <- p + stat_summary(geom="ribbon",fun.ymin="min",fun.ymax="max",alpha=0.1)
p <- p + stat_summary(geom="ribbon", fun.data="median_hilow",alpha=0.2,fill="blue")
p <- p + stat_summary(geom="ribbon", fun.data="mean_cl_boot",alpha=0.2,fill="red")
p <- p + geom_line(aes(group=id),size=0.1,alpha=0.1)
p <- p + labs(x="Year",y="Depletion") + ylim(0,1)
print(p + .THEME)

# posts.df <- posts.df %>% gather(key,ssb,-id,-wts)

# df<- with(hake0,{
# 	df <- data.frame(S,code=code,clr='black')
# 	pdf <- subset(df,code==0)
# 	ndf <- subset(df,code!=0)
# 	ndf$clr <- 'red'
# 	return(rbind(pdf,ndf))
# })

# # ggpairs(df,1:3,color='clr')
# # p <- ggpairs(df,1:3,color='clr',upper = list(continuous = "density", combo = "box"))
# p <- ggpairs(df,1:3,color='clr',upper = "blank",alpha=0.2)
# print(p + .THEME)


# Distribution for MSY
# msy1 <- hake0$S[hake0$code==0,3]


# df<-with(hake0,{
# 	df <- data.frame(Year=data$year,Depl=ps.dt[,code==0])
# 	return(df)
# })

# gdf <- df %>% gather(Year,value="Depletion") 
# colnames(gdf) <- c("Year","Sample","Depletion")


# p <- ggplot(gdf,aes(x=Year,y=Depletion)) 
# p <- p + stat_summary(geom="ribbon",fun.ymin="min",fun.ymax="max",alpha=0.1)
# p <- p + stat_summary(geom="ribbon", fun.data="median_hilow",alpha=0.2)
# p <- p + stat_summary(geom="ribbon", fun.data="mean_se",alpha=0.2,fill="red")
# p <- p + stat_summary(geom="ribbon", fun.data="mean_cl_normal",alpha=0.2,fill="red")
# p <- p + geom_line()

# print(p)

# p  <- ggplot(ndf,aes(y=Spawning.Biomass,x=Year))
# 	p  <- p + stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max",alpha=0.3,fill=clr)
# 	p  <- p + geom_line(aes(Year,y=Spawning.Biomass),data=mdf)
# 	p  <- p + ylim(c(0,max(ndf$Spawning.Biomass)))
# 	return(p + .THEME)



# hake0 <- runModel(hake0)
# plot(hake0)





# # Create a new sID object
# # new_sID(id="Namibian Hake",dfile="NamibianHake.dat")

# # load the hake sID object and assign to hake
# data(NamibianHake)


# # estimate parameters
# hake <- solver(hake)


# # run the age structured model using defaults
# hake <- runModel(hake)

# # Plot the results
# plot(hake)





