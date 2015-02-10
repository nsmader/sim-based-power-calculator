# User interface for simulation-based statistical power calculator app
# For starters, pulled UI code from https://github.com/rstudio/shiny-examples/blob/master/051-movie-explorer/ui.R
library(shiny)
shinyUI(fluidPage(
  titlePanel("Simulation-Based Statistical Power Calculator"),
  fluidRow(
    column(3,
      wellPanel(
        p(actionButton("runSim", "Run simulation", icon("bolt"))) # look here for all icons - http://fontawesome.io/icons/
      ),
      wellPanel(
        h4("Sample Sizes"),
        numericInput("cluster.size", "Number of clusters per arm", 2, min = 1, max = 1000), # 40
        numericInput("cluster.num",  "Sampled units per cluster", 100, min = 1) # 128
      ),
      wellPanel(
        h4("Variance/Covariance"),
        numericInput("cluster.var", "Variance", 0, min = 0),
        numericInput("cluster.ICC", "Intra-Cluster Correlation", 0.028, min = 0)
      ),
      wellPanel(
        h4("Treatment Parameters"),
        numericInput("baseline.prev", "Baseline prevalence", 0.78, min = 0, max = 1),
        numericInput("trt.prev.max", "Max prevalence under treatment", 0.78, min = 0, max = 1),
        numericInput("trt.prev.min", "Min prevalence under treatment", 0.60, min = 0, max = 1),
        numericInput("trt.intv.num", "Number of intervals to test", 5, min = 0, max = 100) # 50
      ),
      wellPanel(
        h4("Power and Simulation Parameters"),
        numericInput("alpha", "Alpha", 0.05, min = 0, max = 1),
        numericInput("n.iter", "Number of Simulation Iterations", 10, min = 0) # 500
      )
    ),
    column(9,
      plotOutput("plot1"),
      tableOutput("table"),
      downloadButton('downloadData', 'Download table of results')
    )
  )
))
