### Server-side code for dispatching runs of simulation-based power calculations ###

library(lme4)
library(shiny)
library(foreach)
library(doParallel)

# See following links for ideas on debugging Shiny apps code
# http://stackoverflow.com/questions/23002712/shiny-what-is-the-option-setting-to-display-in-the-console-the-messages-between
# ... which recommends the command: options(shiny.trace=TRUE)
# http://rstudio.github.io/shiny/tutorial/#run-and-debug
#options(shiny.trace=TRUE)

shinyServer(function(input, output){
  
  registerDoParallel(cores = detectCores())
  
  Results <- reactive({
    input$runSim # NSM: This creates dependency (i.e. will rerun this code) every time the "run sim" button is clicked.
                 #      The trick is to isolate this run from all other changes of parameters, until the user is ready 
                 #        to run. I'm not quite sure how to do this but, somehow, this example does it: https://github.com/rstudio/shiny-examples/blob/master/060-retirement-simulation/server.r
    
    # Isolate all of the following assignments to not allow them to run until the "Run Sim" button is clicked
    # Great description of isolation: http://shiny.rstudio.com/articles/isolation.html
    cluster.num  = isolate(as.integer(input$cluster.num))
    cluster.size = isolate(as.integer(input$cluster.size))
    
    cluster.ICC = isolate(as.numeric(input$cluster.ICC))
    cluster.var = ( cluster.ICC*pi^2/3)/(1-cluster.ICC)
    
    # Make modifications if not using a clustered design
    if(input$clusterDesign == F) {
      cluster.num <- 1
      cluster.ICC <- 0
    }
    
    nSim   = isolate(as.numeric(input$n.iter))
    alpha  = isolate(as.numeric(input$alpha))
    print(paste("nSim =", nSim))
    output$iter  <- renderText(paste("Number of simulations is", nSim))
    output$cores <- renderText(paste("Number of utilized cores is", getDoParWorkers()))
    
    #########################
    ### Set up simulation ###
    #########################
    
    baseline.prev = isolate(as.numeric(input$baseline.prev))
    if (input$trtSpec == 'Prevalence'){
      
      trt.prev.min = isolate(as.numeric(input$trt.prev)[1])
      trt.prev.max = isolate(as.numeric(input$trt.prev)[2])
      trt.vals.num = isolate(as.numeric(input$trt.vals.num))
      
      treat.prev = rev(seq (trt.prev.min, trt.prev.max, length.out = trt.vals.num))
      or.list = treat.prev/(1-treat.prev) / ( baseline.prev/(1-baseline.prev))  
      
      outLabel = "Treatment Prevalence"
    } else if (input$trtSpec == 'Odds R') {
      or.vals     = isolate(as.numeric(input$or.list))
      or.vals.num = isolate(as.numeric(input$or.vals.num))
      print(paste("or.vals is ", or.vals))
      or.list = seq(or.vals[2], or.vals[1], length.out = or.vals.num)
      
      outLabel = "Odds Ratio"
    }
    
    cluster.id = rep(1:(cluster.num*2), each = cluster.size)
    treat.id = rep ( c(0,1), each = cluster.num*cluster.size)
    
    Results = NULL
    
    ### Begin simulation    
    for (or.i in or.list){
    	vIsSig <- foreach(1:nSim, .combine = rbind) %dopar% {
        library(lme4)
#       vIsSig <- NULL
#       for(k in 1:n.iter){
    		cluster.re <- rnorm(cluster.num*2, 0, sqrt (cluster.var) ) # XXX Replace this with analytical calculations for non-clustered designs
    		mu = cluster.re[cluster.id] + log(baseline.prev/(1-baseline.prev)) + treat.id*log(or.i)
    		p = exp(mu)/(1+exp(mu))
    		Y = rbinom (length(mu), 1, p)
    		fit = glmer(Y~treat.id + (1|cluster.id), family = "binomial") 
    		ests = coef(summary(fit)) 
    		ests = ests[ grep("treat.id", row.names(ests)),]
        isSig <- ests[4] < alpha
    		return(isSig) # Column 4 has the p-value for the treatment variable
#         vIsSig <- rbind(vIsSig, isSig)
    	} #End of iteration	
      counter <- sum(vIsSig)
  
      xLabel <- ifelse(input$trtSpec == 'Odds ratio under treatment', or.i, treat.prev[which(or.i == or.list)])
    	Results = rbind (Results, c(xLabel, round(counter/nSim, 3) ))
    
    } #End of ORs
    colnames(Results) <- c(outLabel, "Power")
    rownames(Results) <- NULL
    Results
  })
  
  output$plot1 <- renderPlot({  
    plot (Results(), type = "l", xlab = colnames(Results())[1], ylab = "Power")
    # XXX Put a line on the graph at 0.8
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