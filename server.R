### Server-side code for dispatching runs of simulation-based power calculations ###

library(lme4)
library(shiny)
library(shinyapps)
library(foreach)
library(doParallel)

setwd("~/GitHub/sim-based-power-calculator") # /!\ Should remove this before deploying
source("./power-fns.R")

# See following links for ideas on debugging Shiny apps code
# http://stackoverflow.com/questions/23002712/shiny-what-is-the-option-setting-to-display-in-the-console-the-messages-between
# ... which recommends the command: options(shiny.trace=TRUE)
# http://rstudio.github.io/shiny/tutorial/#run-and-debug
#options(shiny.trace=TRUE)

shinyServer(function(input, output){
  
  Results <- reactive({
    
    # Isolate all of the following assignments to not allow them to run until the "Run Sim" button is clicked
    # Great description of isolation: http://shiny.rstudio.com/articles/isolation.html
    alpha  = reactive(as.numeric(input$alpha))
    resid.var = reactive(as.numeric(input$resid.var))
    
    # Make modifications if not using a clustered design
    if (input$clusterDesign == T){
      cluster.ICC = reactive(as.numeric(input$cluster.ICC))
      cluster.var = ( cluster.ICC*pi^2/3)/(1-cluster.ICC)
#       cluster.num  = isolate(as.integer(input$cluster.num))
#       cluster.size = isolate(as.integer(input$cluster.size))
    } else {
      cluster.num <- 1
      cluster.ICC <- 0
    }
    
    if (input$outcomeType == "cts") {

      if (input$clusterDesign == T){
        
        # 1. specify power and effect size --> receive cluster size vs. # clusters (discrete)
          # /!\ Minor note--could redo this sequence of if()s to be a switch()
          if (input$clusterRequest == "cluster size vs. # clusters"){
            power       <- reactive(as.numeric(input$power))
            effect.size <- reactive(as.numeric(input$effect.size))
            clusterNums = 1:50
            #plotTable <- data.frame(clusterNums, clusterSize = ctsClus_getClusterSize_clusterNum ( clusterNums ))
              # XXX Not yet defined            
            # Plot results curve
          }
           
        # 2. specify cluster size and # clusters --> receive power vs. effect size
          if (input$clusterRequest == "power vs. effect size"){
            cluster.size <- reactive(as.numeric(input$cluster.size))
            cluster.num  <- reactive(as.numeric(input$cluster.num))
            effectSizes = seq(0, 2, by = 0.1)
            plotTable <- data.frame(effectSizes, power = ctsClus_getpower_effectSize(EffectSizes))
            # Plot results curve
            # Add text that specifies "To achieve 80% power you need an effect size of ___"
          }

        # 3. specify power and # clusters -------> receive effect size vs. cluster size
          if (input$clusterRequest == "effect size vs. cluster size"){
            power        <- reactive(as.numeric(input$power))
            cluster.num  <- reactive(as.numeric(input$cluster.num))
            outTable <- data.frame(NULL)
            for (clusterSize in clusterSizeRange){
              outTable <- rbind(outTable, 
                                data.frame(clusterSize = clusterSize,
                                           effectSize = ctsClus_getEffectSize(power, clusterNum, clusterSize)))
            }
            # Plot results curve
          }

        # 4. specify power and cluster size ------> receive effect size vs. # clusters (discrete)
          if (input$clusterRequest == "effect size vs. # clusters"){
            cluster.size <- reactive(input$cluster.size)
            power        <- reactive(input$power)
            clusterNums <- 1:50
            plotTable <- data.frame(clusterNum, effectSize = ctsClus_getEffectSize_clusterNum(1:50))
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
