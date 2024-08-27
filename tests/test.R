################################################
############ Well Log Examples #################
################################################

library(changepoint.influence)

#### Load the data in the R package changepoint.influence ####
data(welldata)
welllog = welldata[1001:2000] # Extract the mid section of the data as analyzed in other papers
n = length(welllog)
var = NULL; for (i in 30:1000){var[i]=var(welllog[(i-29):i])} # rescale the data to have unit variance
welllogs = welllog/sqrt(median(var, na.rm = T))

#### Apply PELT to the welllog data ####
out.PELT = cpt.mean(welllogs, method = 'PELT')

#### Calculate Influence ####
welllogs.inf = influence(out.PELT) # with default k=1
welllogs.inf.k = influence(out.PELT, k=2) # with default multiple (k larger than one) modified data points
set.seed(1);welllogs.inf.random = influence(out.PELT, modify="random", n.modify=100) # random indices with default k=1
set.seed(10);welllogs.inf.random.k = influence(out.PELT, k=3, modify="random", n.modify=100) # random with default multiple (k larger than one) modified data points
set.seed(100);welllogs.inf.stratified = influence(out.PELT, modify="stratified", n.modify=500) # stratified indices with default k=1
set.seed(1000);welllogs.inf.stratified.k = influence(out.PELT, k=5, modify="stratified", n.modify=500) # stratified with default multiple (k larger than one) modified data points
welllogs.inf.user = influence(out.PELT, modify=c(23,64,239,792)) # random indices with default k=1
welllogs.inf.user.k = influence(out.PELT, k=10, modify=c(23,64,239,792)) # random with default multiple (k larger than one) modified data points


# Stability Overview
# Original k=1
StabilityOverview(welllogs.inf, las = 1, 
                  legend.args = list(display = TRUE, x = "bottomright", y = NULL, cex = 1.5, bty = "n", horiz = FALSE, xpd = FALSE), ylab = 'Nuclear-Magnetic Response') 

# Random k=1
StabilityOverview(welllogs.inf.random, las = 1, 
                  legend.args = list(display = TRUE, x = "bottomright", y = NULL, cex = 1.5, bty = "n", horiz = FALSE, xpd = FALSE), ylab = 'Nuclear-Magnetic Response') 


# k=2  and original
StabilityOverview(welllogs.inf.k, las = 1,
                  legend.args = list(display = TRUE, x = "bottomright", y = NULL, cex = 1.5, bty = "n", horiz = FALSE, xpd = FALSE), ylab = 'Nuclear-Magnetic Response')

# k=2 and random
StabilityOverview(welllogs.inf.random.k, las = 1,
                  legend.args = list(display = TRUE, x = "bottomright", y = NULL, cex = 1.5, bty = "n", horiz = FALSE, xpd = FALSE), ylab = 'Nuclear-Magnetic Response')



#### Location Stability plot ####
# original k=1
LocationStability(welllogs.inf, type = 'Difference', cpt.lwd = 4, las = 1)

# Random k=1
LocationStability(welllogs.inf.random, type = 'Difference', cpt.lwd = 4, las = 1)

# k=2  and original
LocationStability(welllogs.inf.k, type = 'Difference', cpt.lwd = 4, las = 1)

# k=2 and random
LocationStability(welllogs.inf.random.k, type = 'Difference', cpt.lwd = 4, las = 1)



#### Parameter Stability plot ####
# original k=1
ParameterStability(welllogs.inf, las = 1,ylab = 'Nuclear-Magnetic Response')

# Random k=1
ParameterStability(welllogs.inf.random, las = 1,ylab = 'Nuclear-Magnetic Response')

# k=2  and original
ParameterStability(welllogs.inf.k, las = 1,ylab = 'Nuclear-Magnetic Response')

# k=2 and random
ParameterStability(welllogs.inf.random.k, las = 1,ylab = 'Nuclear-Magnetic Response')


#### Influence Map ####

# original k=1
InfluenceMap(welllogs.inf, include.data = T,ylab = 'Nuclear-Magnetic\n Response')

# Random k=1
InfluenceMap(welllogs.inf.random, ylab = 'Nuclear-Magnetic Response')

# k=2  and original
InfluenceMap(welllogs.inf.k, las = 1,ylab = 'Nuclear-Magnetic Response')

# k=2 and random
InfluenceMap(welllogs.inf.random.k, las = 1,ylab = 'Nuclear-Magnetic Response')

welllogs.inf = influence(out.PELT, method = "delete")
InfluenceMap(welllogs.inf, include.data = T,ylab = 'Nuclear-Magnetic\n Response')

welllogs.inf = influence(out.PELT, method = "outlier")
InfluenceMap(cpts(out.PELT), welllogs.inf, data = welllogs, include.data = T, 
             ylab='Nuclear-Magnetic\n Response')





################################################
############ Simulated Examples ################
################################################

#### Generate Simulated data example ####
set.seed(30)
x = c(rnorm(50), rnorm(50, mean = 5), rnorm(1, mean = 15), rnorm(49, mean = 5), rnorm(50, mean = 4))
xcpt = cpt.mean(x,method='PELT') # Get the changepoints via PELT

#### Stability Dashboard ####
x.inf = influence(xcpt)
StabilityOverview(x, cpts(xcpt), x.inf, las=1, 
                  legend.args = list(display = TRUE, x ="topright", y = NULL, cex = 1.5, bty = "n", horiz = FALSE, xpd = FALSE)) 


#### Location Stability plot ####
LocationStability(cpts(xcpt), type = 'Difference', x.inf, cpt.lwd = 4, las = 1)

#### Parameter Stability plot ####
ParameterStability(x.inf, original.mean = rep(param.est(xcpt)$mean, times=diff(c(0,xcpt@cpts))), las = 1)

#### Influence Map ####
x.inf = influence(xcpt, method = "delete")
InfluenceMap(cpts(xcpt), x.inf, data = x, include.data = T)
x.inf = influence(xcpt, method = "outlier")
InfluenceMap(cpts(xcpt), x.inf, data=x, include.data = T)