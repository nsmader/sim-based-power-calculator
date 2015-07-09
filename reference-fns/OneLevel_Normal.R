### Two-level Logistic Random Effect Model ###

rm (list = ls() )

########################
### Input parameters ###
########################
library (lme4) 
library (spam)

sample.size = 100
alpha = 0.05

resid.var = 4
eff.max = 2
eff.min = -2
eff.by = 0.1

sim = FALSE  #Perform simulation? 
n.iter = 1000

#########################
### Set up simulation ###
#########################

eff.list = rev(seq (eff.min, eff.max, eff.by))
treat.id = rep ( c(0,1), each = sample.size/2)

###Set up design matrix
X = as.spam( cbind (1, treat.id) )

###Estimate standard error
V = resid.var*diag.spam(sample.size)
SE = sqrt(solve( t(X)%*%solve(V)%*%X)[2,2])

Results = NULL

#setwd ("H:/Projects/Matt Freeman/TwoLevel")
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
      mu = treat.id*eff.i
      Y = mu + rnorm (sample.size, 0, sqrt(resid.var))
      fit = lm (Y~treat.id)
      ests = coef(summary(fit))
      ests = ests[ grep("treat.id", row.names(ests)),]
      if (abs(ests[3]) > qnorm (1-alpha/2) ) { counter = counter + 1 }
    } #End of iteration	
    Results = rbind (Results, c(eff.i, round(counter/n.iter, 3) ))
  }
  
} #End of Effect Size

plot (Results, type = "l", xlab = "Effect Size", ylab = "Power")


