### Server-side code for dispatching runs of simulation-based power calculations ###

library(lme4)
library(shiny)
library(shinyapps)
library(foreach)
library(doParallel)

# See following links for ideas on debugging Shiny apps code
# http://stackoverflow.com/questions/23002712/shiny-what-is-the-option-setting-to-display-in-the-console-the-messages-between
# ... which recommends the command: options(shiny.trace=TRUE)
# http://rstudio.github.io/shiny/tutorial/#run-and-debug
#options(shiny.trace=TRUE)

shinyServer(function(input, output){
  
  Results <- reactive({
    
    # Isolate all of the following assignments to not allow them to run until the "Run Sim" button is clicked
    # Great description of isolation: http://shiny.rstudio.com/articles/isolation.html
    alpha  = isolate(as.numeric(input$alpha))
    
    # Make modifications if not using a clustered design
    # XXX Writing pseudo-code here
    if (input$clusterDesign == T){
      cluster.ICC = isolate(as.numeric(input$cluster.ICC))
      cluster.var = ( cluster.ICC*pi^2/3)/(1-cluster.ICC)
      cluster.num  = isolate(as.integer(input$cluster.num))
      cluster.size = isolate(as.integer(input$cluster.size))
    } else {
      cluster.num <- 1
      cluster.ICC <- 0
    }
    
    if (input$outcomeType == "cts") {
      # Bring in outcome under non-intervention
      # Power, effect size, cluster size
      if (input$clusterDesign == T){
        
        # 1. specify power and effect size ------> receive cluster size vs. # clusters (discrete)
          if (input$clusterRequest == "getClusterSize_clusterNum"){
            outTable <- data.frame(NULL)
            for (clusterNum in clusterNumRange){
              outTable <- rbind(outTable, 
                                data.frame(clusterNum = clusterNum,
                                           clusterSize = ctsClus_getClusterSize(power, clusterNum, effectSize)))
            }
            # Plot results curve
          }
           
        # 2. specify cluster size and # clusters -> receive power vs. effect size
          if (input$clusterRequest == "getPower_effectSize"){
            outTable <- data.frame(NULL)
            for (power in powerRange){
              outTable <- rbind(outTable, 
                                data.frame(power = power,
                                           effectSize = ctsClus_getEffectSize(power, clusterNum, clusterSize)))
            }
            # Plot results curve
            # Add text that specifies "To achieve 80% power you need an effect size of ___"
          }

        # 3. specify power and # clusters -------> receive effect size vs. cluster size
          if (input$clusterRequest == "getEffectSize_clusterSize"){
            outTable <- data.frame(NULL)
            for (clusterSize in clusterSizeRange){
              outTable <- rbind(outTable, 
                                data.frame(clusterSize = clusterSize,
                                           effectSize = ctsClus_getEffectSize(power, clusterNum, clusterSize)))
            }
            # Plot results curve
          }

        # 4. specify power and cluster size ------> receive effect size vs. # clusters (discrete)
          if (input$clusterRequest == "getEffectSize_clusterNum"){
            outTable <- data.frame(NULL)
            for (clusterNum in clusterNumRange){
              outTable <- rbind(outTable, 
                                data.frame(clusterSize = clusterSize,
                                           effectSize = ctsClus_getEffectSize(power, clusterNum, clusterSize)))
            }
            # Plot results curve
          }
      } else {
        # 1. specify effect size, receive power vs. sample size
        ctsIndiv_getPower(sampleSize, effectSize)
        # 2. specify sample size, receive power vs. effect size
        ctsIndiv_getEffectSize(sampleSize, power)
        # 3. specify power, receive effect size vs. sample size
        ctsIndiv_getSampleSize(effectSize, power)
      }
    
      # Bring in parameters appropriate to--and run--power calculations for cts outcome
    } else if (input$outcomeType == "binary"){
      if (input$clusterDesign == T){
          
        # Run simulation with clustering according to specified parameters
        input$runSim # NSM: This creates dependency (i.e. will rerun this code) every time the "run sim" button is clicked.
                     #      The trick is to isolate this run from all other changes of parameters, until the user is ready 
                     #        to run. I'm not quite sure how to do this but, somehow, this example does it: https://github.com/rstudio/shiny-examples/blob/master/060-retirement-simulation/server.r
      } else {
        # Run unclustered analysis of binary outcome
      }
    }
    
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