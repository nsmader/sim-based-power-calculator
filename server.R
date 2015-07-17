### Server-side code for dispatching runs of simulation-based power calculations ###

library(lme4)
library(shiny)
library(shinyapps)
library(foreach)
library(doParallel)
library(ggplot2)

setwd("~/GitHub/sim-based-power-calculator") # /!\ Should remove this before deploying
source("./power-fns.R")

# See following links for ideas on debugging Shiny apps code
# http://stackoverflow.com/questions/23002712/shiny-what-is-the-option-setting-to-display-in-the-console-the-messages-between
# ... which recommends the command: options(shiny.trace=TRUE)
# http://rstudio.github.io/shiny/tutorial/#run-and-debug
#options(shiny.trace=TRUE)

shinyServer(function(input, output){
  
  alpha     = reactive(as.numeric(input$alpha))
  resid.var = reactive(as.numeric(input$resid.var))
  
  # Make modifications if not using a clustered design
  cluster.num <- reactive(ifelse(input$clusterDesign == FALSE, 1, as.numeric(input$cluster.num)))
  cluster.ICC <- reactive(ifelse(input$clusterDesign == FALSE, 0, as.numeric(input$cluster.ICC)))
  #cluster.var <- reactive(ifelse(input$clusterDesign == FALSE, 0, cluster.ICC()*pi^2/3)/(1-cluster.ICC()))
    # This is functional relationship between ICC and variance is for logistic errors...
  cluster.var <- reactive(ifelse(input$clusterDesign == FALSE, 0, cluster.ICC()*resid.var()/(1-cluster.ICC())))
  
  output$plotOut <- renderPlot({
    input$updateCalc # This reruns this code when the "updateCalc" button in the UI is clicked
  {
  if (input$outcomeType == "Continuous"){
    
    if (input$clusterDesign == T){
      
      # 1. specify power and effect size --> receive cluster size vs. # clusters (discrete)
        # /!\ Minor note--could redo this sequence of if()s to be a switch()
        if (input$clusterRequest == "cluster size vs. # clusters"){
          power       <- reactive(as.numeric(input$power))
          effect.size <- reactive(as.numeric(input$effect.size))
          clusterNums = 1:50
          
          #plotTable <- data.frame(clusterNum = clusterNums, clusterSize = ctsClus_getClusterSize_clusterNum ( clusterNums ))
            # XXX Not yet defined
#             plotOut <- ggplot(plotTable(), aes(x = clusterNum, y = clusterSize)) +
#               geom_line(colour = "blue", size = 1) +
#               ggtitle(paste("Cluster Size vs. # of Clusters\nto obtain at least", sprintf("%3.2f", power()), "with effect size", sprintf("%3.2f", effect.size()))) +
#               xlab("# Clusters") +
#               ylab("Cluster Size")
        }
         
      # 2. specify cluster size and # clusters --> receive power vs. effect size
        if (input$clusterRequest == "power vs. effect size"){
          cat("In power vs. effect size")
          cluster.size <- reactive(as.numeric(input$cluster.size))
          cluster.num  <- reactive(as.numeric(input$cluster.num))
          effectSizes = seq(0, 2, by = 0.05)
          
          plotTable <- reactive(data.frame(effectSize = effectSizes,
                                           power = ctsClus_getpower_effectSize(effectSizes,
                                                                               cluster.size = cluster.size(),
                                                                               cluster.num = cluster.num(),
                                                                               cluster.var = cluster.var(),
                                                                               resid.var = resid.var(),
                                                                               alpha = alpha())))
          myPlot <- ggplot(plotTable(), aes(x = effectSize, y = power)) +
                      geom_line(colour = "blue", size = 1) +
                      ggtitle(paste("Power vs. Effect Size\nfor", cluster.num(), "clusters of", cluster.size(), "observations each")) +
                      xlab("Effect Size") + ylab("Power") + geom_hline(yintercept = 0.80)
          return(myPlot)
          # Add text that specifies "To achieve 80% power you need an effect size of ___"

        }
      # 3. specify power and # clusters --> receive effect size vs. cluster size
        if (input$clusterRequest == "effect size vs. cluster size"){
          power        <- reactive(as.numeric(input$power))
          cluster.num  <- reactive(as.numeric(input$cluster.num))
          clusterSizes <- seq(10, 200, by = 5)
          
          plotTable <- reactive(data.frame(clusterSize = clusterSizes,
                                           effectSize = ctsClus_getEffectSize_clusterSize(clusterSizes,
                                                                                          cluster.num = cluster.num(),
                                                                                          power = power(),
                                                                                          cluster.var = cluster.var(),
                                                                                          resid.var = resid.var(),
                                                                                          alpha = alpha())))
          myPlot <- ggplot(plotTable(), aes(x = clusterSize, y = effectSize)) + geom_line(colour = "blue", size = 1) +
            ggtitle(paste("Effect Size vs. Cluster Size\nfor", cluster.num(), "clusters, for power =", sprintf("%3.2f", power()))) +
            xlab("Cluster Size") + ylab("Effect Size")
          return(myPlot)
        }

      # 4. specify power and cluster size --> receive effect size vs. # clusters (discrete)
        if (input$clusterRequest == "effect size vs. # clusters"){
          cluster.size <- reactive(input$cluster.size)
          power        <- reactive(input$power)
          clusterNums <- 1:50
          plotTable <- reactive(data.frame(clusterNum = clusterNums,
                                           effectSize = ctsClus_getEffectSize_clusterNum(clusterNums,
                                                                                         cluster.size = cluster.size(),
                                                                                         power = power(),
                                                                                         cluster.var = cluster.var(),
                                                                                         resid.var = resid.var(),
                                                                                         alpha = alpha())))
          myPlot <- ggplot(plotTable(), aes(x = clusterNum, y = effectSize)) + geom_line(colour = "blue", size = 1) +
            ggtitle(paste("Effect Size vs. # of Clusters\nfor", cluster.size(), "obs per cluster, for power =", sprintf("%3.2f", power()))) +
            xlab("# of Clusters") + ylab("Effect Size")
          return(myPlot)
        }
    } else { # i.e. if input$clusterDesign != T
      cluster.num <- 1
      
      # 1. specify effect size, receive power vs. sample size
        if (input$indvRequest == "power vs. sample size"){
          effect.size <- reactive(input$effect.size)
          sampleSizes <- seq(10, 1000, by = 5)
          
#           plotTable <- reactive(data.frame(sampleSize = sampleSizes, power = ctsClus_getpower_clusterSize(sampleSizes)))
#           XXX Not yet developed
#           plotOut <- ggplot(plotTable(), aes(x = sampleSize, y = power)) + geom_line(colour = "blue", size = 1) +
#             ggtitle(paste("Power vs. Sample Size\nfor effect size of", sprintf("%3.2f", effect.size()))) +
#             xlab("Effect Size") + ylab("Power") + geom_hline(yintercept = 0.80)  
        }        

      # 2. Specify sample size, receive power vs. effect size
        if (input$indvRequest == "power vs. effect size"){
          sample.size <- reactive(input$sample.size)
          effectSizes = seq(0, 2, by = 0.05)
        
          plotTable <- reactive(data.frame(effectSize = effectSizes,
                                           power = ctsClus_getpower_effectSize(effectSizes,
                                                                               cluster.num = cluster.num,
                                                                               cluster.size = sample.size(),
                                                                               cluster.var = cluster.var(),
                                                                               resid.var = resid.var(),
                                                                               alpha = alpha())))

          myPlot <- ggplot(plotTable(), aes(x = effectSize, y = power)) +
            geom_line(colour = "blue", size = 1) +
            ggtitle(paste("Power vs. Sample Size\nfor effect size of", sample.size())) +
            xlab("Effect Size") +
            ylab("Power") +
            geom_hline(yintercept = 0.80)
  
          return(myPlot)
        }

      # 3. Specify power, receive effect size vs. sample size
        if (input$indvRequest == "effect size vs. sample size"){
          power <- reactive(as.numeric(input$power))
          sampleSizes <- seq(10, 1000, by = 100)
          
          plotTable <- reactive(data.frame(sampleSize = sampleSizes,
                                           effectSize = ctsClus_getEffectSize_clusterSize(clusterSize.list = sampleSizes,
                                                                                          cluster.num = cluster.num,
                                                                                          power = power(),
                                                                                          cluster.var = cluster.var(),
                                                                                          resid.var = resid.var(),
                                                                                          alpha = alpha())))
          
          myPlot <- ggplot(plotTable(), aes(x = sampleSize, y = effectSize)) +
            geom_line(colour = "blue", size = 1) +
            ggtitle(paste("Effect Size vs. Sample Size\nfor power = ", sprintf("%3.2f", power()))) +
            xlab("Sample Size") +
            ylab("Effect Size")
  
          return(myPlot)
        }

    } # End of code pertaining to unclustered continuous outcome designs
  
  } else if (input$outcomeType == "Binary"){ # Code for binary outcome designs
    
    if (input$clusterDesign == T){
        
      # Run simulation with clustering according to specified parameters
      Results <- reactive({
        # Isolate all of the following assignments to not allow them to run until the "Run Sim" button is clicked
        # Great description of isolation: http://shiny.rstudio.com/articles/isolation.html
        #         input$runSim # NSM: This creates dependency (i.e. will rerun this code) every time the "run sim" button is clicked.
                   #      The trick is to isolate this run from all other changes of parameters, until the user is ready 
                   #        to run. I'm not quite sure how to do this but, somehow, this example does it: https://github.com/rstudio/shiny-examples/blob/master/060-retirement-simulation/server.r
      })

    } else { # Code for unclustered designs
      # Run unclustered analysis of binary outcome
    }
  } # End of binary designs
  } # This is the hierarchy of {cts/bin, clustered, requeset}

  })

  # Table output
#   output$table <- renderTable(Results())
  
#   output$downloadData <- downloadHandler(
#     filename = "sim-results-out.csv",
#     content = function(file) {
#       write.csv(Results(), file, row.names = FALSE)
#     }
#   )

})
