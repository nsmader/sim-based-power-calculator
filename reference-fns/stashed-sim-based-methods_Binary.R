
nSim   = isolate(as.numeric(input$n.iter))

#########################
### Set up simulation ###
#########################
  
  baseline.prev = isolate(as.numeric(input$baseline.prev))
  if (input$trtSpec == 'Prevalence'){
    
    trt.prev.min = isolate(as.numeric(input$trt.prev)[1])
    trt.prev.max = isolate(as.numeric(input$trt.prev)[2])
    trt.vals.num = isolate(as.numeric(input$trt.vals.num))
    
    treat.prev = seq(trt.prev.min, trt.prev.max, length.out = trt.vals.num)
    or.list = treat.prev/(1-treat.prev) / ( baseline.prev/(1-baseline.prev))  
    
    outLabel = "Treatment Prevalence"
    labelVals <- treat.prev
  } else if (input$trtSpec == 'Odds Ratio') {
    or.vals     = isolate(as.numeric(input$or.list))
    or.vals.num = isolate(as.numeric(input$or.vals.num))
    or.list = seq(or.vals[1], or.vals[2], length.out = or.vals.num)
    
    outLabel = "Odds Ratio"
    labelVals <- or.list
  }
  
  cluster.id = rep(1:(cluster.num*2), each = cluster.size)
  treat.id = rep ( c(0,1), each = cluster.num*cluster.size)
  
  Results = NULL
  
  ### Begin simulation    
  registerDoParallel(cores = detectCores() - 1)
  
  for (or.i in or.list){ # NSM: Could farm this into the foreach by making a runlist permuting or.list and nSim, whose rows are fed into the foreach
    vIsSig <- foreach(1:nSim, .combine = rbind) %dopar% {
      library(lme4)
#       vIsSig <- NULL # Initialize the vector of indicators 
#       for(k in 1:n.iter){
      # XXX The code that follows is just one type of research design, and one type of power calculation of interest
      #     to the user. The below should be made to be modular to different designs or different power calcs, e.g.
      #     analytical calculations for non-clustered designs, etc.
      # The code below could be written as a function to be called with:
      #   cluster.id, treat.id, cluster.num, cluster.var, baseline.prev, and or.i
  		cluster.re <- rnorm(cluster.num*2, 0, sqrt (cluster.var) )
  		mu = cluster.re[cluster.id] + log(baseline.prev/(1-baseline.prev)) + treat.id*log(or.i)
  		p = exp(mu)/(1+exp(mu))
  		Y = rbinom (length(mu), 1, p)
  		fit = glmer(Y~treat.id + (1|cluster.id), family = "binomial") 
  		ests = coef(summary(fit)) 
  		ests = ests[ grep("treat.id", row.names(ests)),]
      isSig <- ests[4] < alpha # Column 4 has the p-value for the treatment variable
  		return(isSig)
#         vIsSig <- rbind(vIsSig, isSig)
  	} #End of iteration	
    counter <- sum(vIsSig)

    xLabel <- labelVals[which(or.i == or.list)]
  	Results = rbind (Results, c(xLabel, round(counter/nSim, 3) ))
    Results
  
  } #End of ORs
  #stopCluster(cl)

  colnames(Results) <- c(outLabel, "Power")
  rownames(Results) <- NULL
  Results

