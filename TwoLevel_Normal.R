### Two-level Normal Random Intercept Model ###

rm (list = ls() )

########################
### Input parameters ###
########################
library (lme4) 
library (spam)

cluster.num = 20 #Each arm
cluster.size = 10
alpha = 0.05

#either ICC or variance
cluster.var = NULL
cluster.ICC = 0.5

resid.var = 1
eff.max = 2
eff.min = -2
eff.by = 0.1

sim = FALSE  #Perform simulation? 
n.iter = 500

#########################
### Set up simulation ###
#########################

if ( is.null(cluster.var) ) {cluster.var = cluster.ICC*resid.var/(1-cluster.ICC)}

eff.list = rev(seq (eff.min, eff.max, eff.by))
cluster.id = rep(1:(cluster.num*2), each = cluster.size)
treat.id = rep ( c(0,1), each = cluster.num*cluster.size)

###Set up design matrix
X = as.spam( cbind (1, treat.id) )

###Estimate standard error
V = resid.var*diag.spam(2*cluster.num*cluster.size) + kronecker.spam(diag(2*cluster.num), matrix(cluster.var, cluster.size, cluster.size))
SE = sqrt(solve( t(X)%*%solve(V)%*%X)[2,2])

Results = NULL

setwd ("H:/Projects/Matt Freeman/TwoLevel")
### Begin simulation  
for (eff.i in eff.list){
	
  print (paste("Effect Size = ", eff.list[ which(eff.i == eff.list)] ))
  
  if (sim == FALSE){
    power = 1+pnorm (qnorm(alpha/2)-eff.i/SE) - pnorm (qnorm(1-alpha/2)-eff.i/SE)
    Results = rbind (Results, c(eff.i, round(power, 3) ))
  }
  
  if (sim == TRUE){
    counter = 0
    for (k in 1:n.iter){
      if (k %% 50 == 0){ print( paste0("Iteration ", k, " of ", n.iter)  )}
      cluster.re = rnorm (cluster.num*2, 0, sqrt (cluster.var) ) 
      mu = cluster.re[ cluster.id ] + treat.id*eff.i
      Y = mu + rnorm (cluster.num*cluster.size*2, 0, sqrt(resid.var))
      fit = lmer (Y~treat.id + (1|cluster.id))
      ests = summary (fit)@coefs
      ests = ests[ grep("treat.id", row.names(ests)),]
      
      if (abs(ests[3]) > qnorm (1-alpha/2) ) { counter = counter + 1 }
    } #End of iteration	
    Results = rbind (Results, c(eff.i, round(counter/n.iter, 3) ))
  }
  
} #End of Effect Size

plot (Results, type = "l", xlab = "Effect Size", ylab = "Power")


