#----------------------
### CONTINUOUS OUTCOMES
#----------------------
  ##Functional form for R to do root-finding for effect size
    helper_effsize <- function (eff.size, cluster.num, cluster.size, power, resid.var, cluster.var, alpha){
      N = cluster.size*cluster.num
      invV = solve(resid.var*diag(cluster.size) + cluster.var)
      a = sum(invV[,1])
      V = solve( cbind ( c(2*N*a, N*a), rep(N*a, 2)) )
      SE = sqrt(V[2,2])
      1+pnorm (qnorm(alpha/2)-eff.size/SE) - pnorm (qnorm(1-alpha/2)-eff.size/SE)-power
    }
    
  ##Function to calculate power
    helper_power <- function (eff.size, cluster.num, cluster.size, resid.var, cluster.var, alpha){
      N = cluster.size*cluster.num
      invV = solve(resid.var*diag(cluster.size) + cluster.var)
      a = sum(invV[,1])
      V = solve( cbind ( c(2*N*a, N*a), rep(N*a, 2)) )
      SE = sqrt(V[2,2])
      1+pnorm (qnorm(alpha/2)-eff.size/SE) - pnorm (qnorm(1-alpha/2)-eff.size/SE)
    }

  #---------------------------
  # Cluster-randomized Designs
  #---------------------------
    
    ctsClus_getEffectSize_clusterNum <- function (clusterNum.list, cluster.size, power, resid.var, cluster.var, alpha){
      f = function(x){ uniroot (helper_effsize, interval = c(0, 100), 
                                                cluster.num = x, 
                                                cluster.size = cluster.size,
                                                power = power,
                                                resid.var = resid.var,
                                                cluster.var = cluster.var,
                                                alpha = alpha)$root}
      sapply(clusterNum.list, function(x){f(x)} )
    }
    
    ctsClus_getEffectSize_clusterSize <- function (clusterSize.list, cluster.num, power, resid.var, cluster.var, alpha){
      f = function(x){ uniroot (helper_effsize, interval = c(0, 100),
                                                cluster.size = x,
                                                cluster.num = cluster.num,
                                                power = power,
                                                resid.var = resid.var,
                                                cluster.var = cluster.var,
                                                alpha = alpha)$root}
      sapply(clusterSize.list, function(x){f(x)} )
    }
    
    ctsClus_getclusterSize_effectSize <- function (effectSize.list, cluster.num, power, resid.var, cluster.var, alpha){
      
      clusterSize.seq = 1:200
      f = function(y){ 
        power.list = sapply(clusterSize.seq, 
                            function(x){helper_power(cluster.size = x,
                                                     eff.size = y,
                                                     cluster.num = cluster.num,
                                                     resid.var = resid.var,
                                                     cluster.var = cluster.var,
                                                     alpha = alpha)} ) 
        check = power.list > power
        if ( sum(check)>=1 ){ clusterSize.seq[min(which(check))]} else{NA}
      }
      sapply(effectSize.list, function(x){f(x)} )
    }  
    
    ctsClus_getclusterNum_effectSize <- function (effectSize.list, cluster.size, power, resid.var, cluster.var, alpha){
    
      clusterNum.seq = 1:200
      f = function(y){ 
        power.list = sapply(clusterNum.seq, 
                            function(x){helper_power(cluster.num = x,
                                                     eff.size = y,
                                                     cluster.size = cluster.size,
                                                     resid.var = resid.var,
                                                     cluster.var = cluster.var,
                                                     alpha = alpha)} ) 
        check = power.list > power
        if ( sum(check)>=1 ){ clusterNum.seq[min(which(check))]} else{NA}
      }
      sapply(effectSize.list, function(x){f(x)} )
    }  
    
    ctsClus_getpower_effectSize <- function (effectSize.list, cluster.num, cluster.size, resid.var, cluster.var, alpha){
      sapply(effectSize.list, function(x){
        helper_power (eff.size = x,
                      cluster.num = cluster.num,
                      cluster.size = cluster.size,
                      resid.var = resid.var,
                      cluster.var = cluster.var, 
                      alpha = alpha) } 
      )
    }  

  ctsClus_getClusterSize_clusterNum <- function (clusterNum.list, effect.size, resid.var, cluster.var, alpha){
    
    clusterSize.seq = seq(1, 200, by = 4)
    f = function(y){ 
      power.list = sapply(clusterSize.seq, 
                          function(x){helper_power(eff.size = effect.size,
                                                   cluster.num = y,
                                                   cluster.size = x,
                                                   resid.var = resid.var,
                                                   cluster.var = cluster.var,
                                                   alpha = alpha)} ) 
      check = power.list > power
      if ( sum(check)>=1 ){ clusterSize.seq[min(which(check))]} else{NA}
    }
    sapply(clusterNum.list, function(x){f(x)} )
  }


  #------------------------------
  # Individual-randomized Designs
  #------------------------------

    # Rather than creating specialized functions, we use the clustered ones and 
    # fill in cluster.var = 0 and cluster.num = 1

#------------------
### BINARY OUTCOMES
#------------------

  #-----------------------------
  # Clustered-randomized designs
  #-----------------------------
  
#   library(foreach)
#   library(doParallel)
#   binClus_getPower(valRange = seq(0.70, 0.90, by = 0.1),
#                    request = "Prevalence",
#                    baseline.prev = 0.8,
#                    cluster.num = 6,
#                    cluster.size = 100,
#                    cluster.var = 0.2,
#                    alpha = 0.05,
#                    n.iter = 50)
  binClus_getPower <- function(valRange, request, baseline.prev, cluster.num, cluster.size, cluster.var, alpha, n.iter = 10){
    
    if (request == "Prevalence"){
      or.list <- valRange/(1-valRange) / ( baseline.prev/(1-baseline.prev)) 
    } else if (request == "Odds Ratio") {
      or.list <- valRange
    }
    cluster.id <- rep(x = 1:(cluster.num*2),
                      each = cluster.size)
    treat.id   <- rep(x = c(0,1),
                      each = cluster.num*cluster.size)
    
    ### Begin simulation    
    cl <- makeCluster(spec = detectCores() - 1)
    registerDoParallel(cl = cl)
    
    powerTable <- data.frame(NULL)
    for (or.i in or.list){ # NSM: Could farm this into the foreach by making a runlist permuting or.list and nSim, whose rows are fed into the foreach
      
      # Run sim iterations in parallel
      vIsSig <- foreach(1:n.iter, .combine = rbind) %do% {
        library(lme4) # Needs to be established for each worker core
  
    		cluster.re <- rnorm(n = cluster.num*2,
    		                    mean = 0,
    		                    sd = sqrt(cluster.var))
    		mu = cluster.re[cluster.id] + log(baseline.prev/(1-baseline.prev)) + treat.id*log(or.i)
    		p = exp(mu)/(1+exp(mu))
    		Y = rbinom (length(mu), 1, p)
    		fit = glmer(formula = Y ~ treat.id + (1|cluster.id),
    		            family = "binomial") 
    		ests = coef(summary(fit)) 
    		ests = ests[grep("treat.id", row.names(ests)), ]
        isSig <- ests[4] < alpha # Column 4 has the p-value for the treatment variable
    		return(isSig)
        
    	} # End of sim iterations
      counter <- sum(vIsSig)
  
      xLabel <- valRange[which(or.i == or.list)]
    	powerTable <- rbind(powerTable, c(xLabel, round(counter/n.iter, 3) ))
    
    } # End of loop across ORs
    stopCluster(cl)
  
    colnames(powerTable) <- c("x", "power")
    rownames(powerTable) <- NULL
    return(powerTable)
  }

  
  