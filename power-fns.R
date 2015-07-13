#----------------------
### CONTINUOUS OUTCOMES
#----------------------
  ##Functional form for R to do root-finding for effect size
    helper_effsize <- function (eff.size, cluster.num, cluster.size, power){
      N = cluster.size*cluster.num
      invV = solve(resid.var*diag(cluster.size) + cluster.var)
      a = sum(invV[,1])
      V = solve( cbind ( c(2*N*a, N*a), rep(N*a, 2)) )
      SE = sqrt(V[2,2])
      1+pnorm (qnorm(alpha/2)-eff.size/SE) - pnorm (qnorm(1-alpha/2)-eff.size/SE)-power
    }
    
  ##Function to calculate power
    helper_power <- function (eff.size, cluster.num, cluster.size){
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
    
    ctsClus_getEffectSize_clusterNum <- function (clusterNum.list, cluster.size, power){
      f = function(x){ uniroot (helper_effsize, interval = c(0, 100), cluster.size, cluster.num = x, power)$root}
      sapply(clusterNum.list, function(x){f(x)} )
    }
    
    ctsClus_getEffectSize_clusterSize <- function (clusterSize.list, cluster.num, power){
      f = function(x){ uniroot (helper_effsize, interval = c(0, 100), cluster.size = x, cluster.num, power)$root}
      sapply(clusterSize.list, function(x){f(x)} )
    }
    
    ctsClus_getclusterSize_effectSize <- function (effectSize.list, cluster.num, power){
      
      clusterSize.seq = 1:200
      f = function(y){ 
        power.list = sapply(clusterSize.seq, 
                            function(x){helper_power(eff.size = y, cluster.num, cluster.size=x )} ) 
        check = power.list > power
        if ( sum(check)>=1 ){ clusterSize.seq[min(which(check))]} else{NA}
      }
      sapply(effectSize.list, function(x){f(x)} )
    }  
    
    ctsClus_getclusterNum_effectSize <- function (effectSize.list, cluster.size, power){
    
      clusterNum.seq = 1:200
      f = function(y){ 
        power.list = sapply(clusterNum.seq, 
                            function(x){helper_power(eff.size = y, cluster.num=x, cluster.size )} ) 
        check = power.list > power
        if ( sum(check)>=1 ){ clusterNum.seq[min(which(check))]} else{NA}
      }
      sapply(effectSize.list, function(x){f(x)} )
    }  
    
    ctsClus_getpower_effectSize <- function (effectSize.list, cluster.num, cluster.size){
      sapply(effectSize.list, function(x){
        helper_power (eff.size=x, cluster.num, cluster.size) } 
      )
    }  


  #------------------------------
  # Individual-randomized Designs
  #------------------------------

#     ctsIndiv_getPower <- function(sampleSize, effectSize){
#       return(runif(1))
#     }
# 
#     ctsIndiv_getEffectSize <- function(sampleSize, power){
#       return(rnorm(1))
#     }
#     ctsIndiv_getSampleSize <- function(effectSize, power){
#       return(round(runif(1)*100, 0))
#     }

#------------------
### BINARY OUTCOMES
#------------------

