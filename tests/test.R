library(changepoint.influence)

set.seed(30) 
x=c(rnorm(50),rnorm(50,mean=5), # green
    rnorm(1,mean=15),rnorm(49,mean=5), # red
    rnorm(50,mean=4)) # orange

xcpt=cpt.mean(x,method='PELT')
cpts(xcpt)

x.inf=influence.generate.PELT(xcpt)

stability.overview(x,cpts(xcpt),x.inf) # generates two plots of data with cpts

location.stability(cpts(xcpt),x.inf) # generates two histograms
location.stability(cpts(xcpt),x.inf,include.data=T,data=x)


influence.map(cpts(xcpt),x.inf)
influence.map(cpts(xcpt),x.inf,include.data=T,data=x)

parameter.stability(x.inf,original.mean=rep(param.est(xcpt)$mean,times=diff(c(0,xcpt@cpts))))
