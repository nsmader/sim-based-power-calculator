### Two-level Logistic Random Effect Model ###

########################
### Input parameters ###
########################
library(lme4) 
library(shiny)
# try(setwd ("H:/Projects/Matt Freeman/TwoLevel"), silent = T)
# try(setwd ("C:/Users/nmader/Documents/GitHub/sim-based-power-calculator"), silent = T)

shinyServer(function(input, output){

  Results <- reactive({
    input$runSim # NSM: This creates dependency (i.e. will rerun this code) every time the "run sim" button is clicked.
                 #      The trick is to isolate this run from all other changes of parameters, until the user is ready 
                 #        to run. I'm not quite sure how to do this but, somehow, this example does it: https://github.com/rstudio/shiny-examples/blob/master/060-retirement-simulation/server.r
    
    # Isolate all of the following assignments to not allow them to run until the "Run Sim" button is clicked
    # Great description of isolation: http://shiny.rstudio.com/articles/isolation.html
    cluster.num = isolate(as.integer(input$cluster.num))
    cluster.size = isolate(as.integer(input$cluster.size))
    
    #either ICC or variance
    cluster.var = isolate({
      v <- as.numeric(input$cluster.var)
      if (v == 0) v <- NA
      v
    })
    cluster.ICC = isolate(as.numeric(input$cluster.ICC))
    
    baseline.prev = isolate(as.numeric(input$baseline.prev))
    trt.prev.max = isolate(as.numeric(input$trt.prev.max))
    trt.prev.min = isolate(as.numeric(input$trt.prev.min))
    trt.intv.num = isolate(as.numeric(input$trt.intv.num))
      # NSM: Howard, I changed this to the number of intervals to test--seemed to me to be easier to specify
    
    n.iter = isolate(as.integer(input$n.iter))
    alpha = isolate(as.numeric(input$alpha))
    
    #########################
    ### Set up simulation ###
    #########################
    
    treat.prev = rev(seq (trt.prev.min, trt.prev.max, length.out = trt.intv.num))
    or.list = treat.prev/(1-treat.prev) / ( baseline.prev/(1-baseline.prev))
    
    cluster.id = rep(1:(cluster.num*2), each = cluster.size)
    treat.id = rep ( c(0,1), each = cluster.num*cluster.size)
    
    if ( is.na(cluster.var) ) {cluster.var = ( cluster.ICC*pi^2/3)/(1-cluster.ICC)}
    
    Results = NULL
    
    ### Begin simulation    
    for (or.i in or.list){
    	#print (paste("Treatment Prevalent = ", treat.prev[ which(or.i == or.list)] ))
    	counter = 0
    	for (k in 1:n.iter){
    		#if (k %% 50 == 0){ print( paste0("Iteration ", k, " of ", n.iter)  )}
    		cluster.re = rnorm (cluster.num*2, 0, sqrt (cluster.var) ) 
    		mu = cluster.re[ cluster.id ] + treat.id*log(or.i)
    		p = exp(mu)/(1+exp(mu))
    		Y = rbinom (length(mu), 1, p)
    		fit = glmer(Y~treat.id + (1|cluster.id), family = "binomial") # NSM: Howard, I updated this since lmer with family = binom is now deprecated
    		ests = coef(summary(fit)) # NSM: Howard, also seemed that the summary(fit)@coefs call was deprecated. This returns the desired result.
    		ests = ests[ grep("treat.id", row.names(ests)),]
    		if (ests[4] < alpha) { counter = counter + 1 }
    	} #End of iteration	
    	Results = rbind (Results, c(treat.prev[ which(or.i == or.list)], round(counter/n.iter, 3) ))
    	#write.csv (Results, file = "Hookworm.csv") 
    
    } #End of ORs
    colnames(Results) <- c("Treatment Prevalence", "Power")
    rownames(Results) <- NULL
    Results
  })
    
  output$plot1 <- renderPlot({  
    plot (Results(), type = "l", xlab = "Treatment Prevalence", ylab = "Power")
  })
  
  # Table output
  output$table <- renderTable(Results())
  
  output$downloadData <- downloadHandler(
    filename = "sim-results-out.csv",
    content = function(file) {
      write.csv(Results(), file, row.names = FALSE)
    }
  )

})