
#rm (list = ls())


############################
##    Helper Functions    ##
############################

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


### Functions to loop through a variable 
ctsClus_getEffectSize_clusterNum <- function (clusterNum.list){
  f = function(x){ uniroot (helper_effsize, interval = c(0, 100), cluster.size, cluster.num = x, power)$root}
  sapply(clusterNum.list, function(x){f(x)} )
}

ctsClus_getEffectSize_clusterSize <- function (clusterSize.list){
  f = function(x){ uniroot (helper_effsize, interval = c(0, 100), cluster.size = x, cluster.num, power)$root}
  sapply(clusterSize.list, function(x){f(x)} )
}

ctsClus_getclusterSize_effectSize <- function (effectSize.list){
  
  clusterSize.seq = 1:200
  f = function(y){ 
    power.list = sapply(clusterSize.seq, 
                        function(x){helper_power(eff.size = y, cluster.num, cluster.size=x )} ) 
    check = power.list > power
    if ( sum(check)>=1 ){ clusterSize.seq[min(which(check))]} else{NA}
  }
  sapply(effectSize.list, function(x){f(x)} )
}  

ctsClus_getclusterNum_effectSize <- function (effectSize.list){

  clusterNum.seq = 1:200
  f = function(y){ 
    power.list = sapply(clusterNum.seq, 
                        function(x){helper_power(eff.size = y, cluster.num=x, cluster.size )} ) 
    check = power.list > power
    if ( sum(check)>=1 ){ clusterNum.seq[min(which(check))]} else{NA}
  }
  sapply(effectSize.list, function(x){f(x)} )
}  

ctsClus_getpower_effectSize <- function (effectSize.list){
  sapply(effectSize.list, function(x){
    helper_power (eff.size=x, cluster.num, cluster.size) } 
  )
}  
  


#######################################
#####           EXAMPLES          #####
#######################################

#Inputs ALWAYS specified by user
resid.var = 1
alpha = 0.05
power = 0.80

#Either ICC or variance
cluster.var = NULL
cluster.ICC = 0.5
if ( is.null(cluster.var) ) {cluster.var = cluster.ICC*resid.var/(1-cluster.ICC)}

### Option 1 ###
# Cluster size fixed
# Effect size as a function of Cluster Num
cluster.size = 20
clusterNums = seq(1, 50, 1)
fit = ctsClus_getEffectSize_clusterNum ( clusterNums )
plot (fit~clusterNums, ylab = c("Effect Size"), xlab = "Cluster Number", type="l", col = 4, ylim = c(0, max(fit)))

### Option 2 ###
# Cluster number fixed
# Effect size as a function of Cluster size
cluster.num = 20
clusterSizes = 1:50
fit = ctsClus_getEffectSize_clusterSize ( clusterSizes )
plot (fit~clusterSizes, ylab = c("Effect Size"), xlab = "Cluster Size", type="l", col = 4, ylim = c(0, max(fit)))

### Option 3 ###
# Cluster size fixed
# Cluster number as a function of effect size
cluster.size = 20
EffectSizes = seq(0, 4, by = 0.2)
fit = ctsClus_getclusterNum_effectSize ( EffectSizes)
plot (fit~EffectSizes, xlab = c("Effect Size"), ylab = "Cluster Number", type="l", col = 4)


### Option 4 ###
# Cluster number is fixed
# Cluster size as a function of effect size
cluster.num = 10
EffectSizes = seq(1, 3, by = 0.1)
fit = ctsClus_getclusterSize_effectSize ( EffectSizes)
plot (fit~EffectSizes, xlab = c("Effect Size"), ylab = "Cluster Size", type="l", col = 4, ylim = c(0, max(fit,na.rm=T)))

### Option 5 ###
# Cluster number is fixed
# Cluster size is fixed
# Power as a function of effect size
cluster.num = 10
cluster.size = 5
EffectSizes = seq(0, 2, by = 0.1)
fit = ctsClus_getpower_effectSize (EffectSizes)
plot (fit~EffectSizes, ylab = "Power", xlab = "Effect Size", type = "l", col = 4)

### Option 6 ###
# Non-clustered case
# Cluster number = 1 
# Set cluster variance = 0
# Effect size as a function of Cluster size
cluster.num = 1
cluster.var = 0
clusterSizes = 1:100
fit = ctsClus_getEffectSize_clusterSize ( clusterSizes )
plot (fit~clusterSizes, ylab = c("Effect Size"), xlab = "Cluster Size", type="l", col = 4, ylim = c(0, max(fit)))

### Option 7 ###
# Non-clustered case
# Cluster number = 1
# Cluster size as a function of effect size
cluster.num = 1
cluster.var = 0
EffectSizes = seq(0, 3, by = 0.25)
fit = ctsClus_getclusterSize_effectSize ( EffectSizes)
plot (fit~EffectSizes, xlab = c("Effect Size"), ylab = "Cluster Size", type="l", col = 4, ylim = c(0, max(fit,na.rm=T)))

### Option 8 ###
# Non-clustered case
# Cluster number = 1
# Cluster size is fixed
# Power as a function of effect size
cluster.num = 1
cluster.size = 20
cluster.var = 0
EffectSizes = seq(0, 2, by = 0.1)
fit = ctsClus_getpower_effectSize (EffectSizes)
plot (fit~EffectSizes, ylab = "Power", xlab = "Effect Size", type = "l", col = 4)
