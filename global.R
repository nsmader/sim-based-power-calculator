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

