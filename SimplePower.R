### Two-level Logistic Random Effect Model ###

########################
### Input parameters ###
########################
library (lme4) 
try(setwd ("H:/Projects/Matt Freeman/TwoLevel"), silent = T)
try(setwd ("C:/Users/nmader/Documents/GitHub/sim-based-power-calculator"), silent = T)

cluster.num = 128 #Each arm
cluster.size = 40
alpha = 0.05

#either ICC or variance
cluster.var = NULL
cluster.ICC = 0.028

baseline.prev = 0.78
trt.prev.max = 0.78
trt.prev.min = 0.60
trt.prev.by = 0.0025

n.iter = 500

#########################
### Set up simulation ###
#########################

treat.prev = rev(seq (trt.prev.min, trt.prev.max, trt.prev.by))
or.list = treat.prev/(1-treat.prev) / ( baseline.prev/(1-baseline.prev))

cluster.id = rep(1:(cluster.num*2), each = cluster.size)
treat.id = rep ( c(0,1), each = cluster.num*cluster.size)

if ( is.null(cluster.var) ) {cluster.var = ( cluster.ICC*pi^2/3)/(1-cluster.ICC)}

Results = NULL

### Begin simulation  
for (or.i in or.list){
	
	print (paste("Treatment Prevalent = ", treat.prev[ which(or.i == or.list)] ))
	counter = 0
	for (k in 1:n.iter){
		if (k %% 50 == 0){ print( paste0("Iteration ", k, " of ", n.iter)  )}
		cluster.re = rnorm (cluster.num*2, 0, sqrt (cluster.var) ) 
		mu = cluster.re[ cluster.id ] + treat.id*log(or.i)
		p = exp(mu)/(1+exp(mu))
		Y = rbinom (length(mu),1, p)
		fit = lmer (Y~treat.id + (1|cluster.id), family = "binomial")
		ests = summary (fit)@coefs
		ests = ests[ grep("treat.id", row.names(ests)),]
		if (ests[4] < alpha) { counter = counter + 1 }
	} #End of iteration	
	Results = rbind (Results, c(treat.prev[ which(or.i == or.list)], round(counter/n.iter, 3) ))
	write.csv (Results, file = "Hookworm.csv") 
} #End of ORs

plot (Results, type = "l", xlab = "Treatment Prevalence", ylab = "Power")


