# User interface for simulation-based statistical power calculator app
# For starters, pulled UI code from https://github.com/rstudio/shiny-examples/blob/master/051-movie-explorer/ui.R
library(shiny)
shinyUI(fluidPage(
  titlePanel("Simulation-Based Statistical Power Calculator"),
  fluidRow(
    column(6, 
      wellPanel(
        h4("Study Design"),
        radioButtons(inputId = "design", label = "",
                     choices = c("RCT/Difference Between Groups", "Single Population Estimate", "LQAS"),
                     selected = NULL, inline = FALSE),
        tags$br(),
        checkboxInput("clusterDesign", "Clustered Study Design?", value = F)
      )
    ),
    column(6,
      wellPanel(
        h4("Outcome Type"),
        radioButtons(inputId = "outcome", label = "",
                     choices = c("Binary", "Continuous", "Count"), selected = NULL, inline = FALSE)
      )
    )
  ),
  fluidRow(
    column(3,
      wellPanel(
        p(actionButton("runSim", "Run simulation", icon("bolt"))) # look here for all icons - http://fontawesome.io/icons/
      ),
      conditionalPanel(
        condition = "input.clusterDesign == true",
        wellPanel(
          h4("Sample Sizes"),
          numericInput("cluster.size", "Number of clusters per arm (m)", 25, min = 1, max = 1000), # 40
          numericInput("cluster.num",  "Number of units per cluster (n)", 30, min = 1) # 128
        ),
        wellPanel(
          h4("Inter-Cluster Correlation"),
          #numericInput("cluster.var", "Variance", 0, min = 0), # Decided not to present this option, since interpretation was unclear to early users
          tags$div(title = withMathJax("The equation for ICC is: $$\\frac{\\sigma^2_{Between}}{\\sigma^2_{Total}}$$"), # 
            numericInput("cluster.ICC", "Amount of Intra-Cluster Correlation (ICC)", 0.028, min = 0),
            p("ICC is the fraction of the total individual variance that is attributable to between-cluster variance. An ICC=0
              approximates an individually randomized controlled study. The larger the ICC is, the more important cluster
              differences are in individual outcomes."),
            withMathJax("The equation for ICC is: $$\\frac{\\sigma^2_{Between}}{\\sigma^2_{Total}}$$")
          )
        )
      ),
      conditionalPanel(
        condition = "input.clusterDesign == false",
        wellPanel(
          h4("Sample Size"),
          numericInput("sample.size", "Sample size per arm", 500, min = 1, max = 10000)
        )
      ),
      wellPanel(
        h4("Treatment Parameters"),
        selectInput("trtSpec", "Method of entering treatment effect", c("Prevalence at baseline and treatment", "Odds ratio under treatment")),
        conditionalPanel(
          condition = "input.trtSpec == 'Prevalence at baseline and treatment'",
          sliderInput("baseline.prev.trt", "Prevalence under control conditions",   min = 0, max = 1, step = 0.01, value = 0.8),
          sliderInput("trt.prev",      "Prevalence under treatment conditions", min = 0, max = 1, step = 0.01, value = c(0.6, 0.8)),
          numericInput("trt.vals.num",  "Number of values to test in this range", 5, min = 0, max = 100), # 50
          p("Prevalence under control conditions can be estimated using either baseline values pilot data or the literature.
            Prevalence number treatment is the estimated minimum prevalence you want to detect.")
        ),
        conditionalPanel(
          condition = "input.trtSpec == 'Odds ratio under treatment'",
          sliderInput("baseline.prev.or", "Prevalence under control conditions",   min = 0, max = 1, step = 0.01, value = 0.8),
          sliderInput("or.list",      "Odds ratio", min = 0, max = 1, step = 0.01, value = c(0.75, 1.00)),
          numericInput("or.vals.num",  "Number of values to test in this range", 5, min = 0, max = 100), # 50
          p("The odds ratio represents the treatment effect--relative to baseline--that you want to detect.") #, where values below zero represent a protective effect, and those above zero represent detrimental effects.
        )
      ),
      wellPanel(
        h4("Power and Simulation Parameters"),
        numericInput("alpha", HTML("Alpha (&alpha;)"), 0.05, min = 0, max = 1),
        numericInput("n.iter", "Number of Simulation Iterations", 10, min = 1) # 500
      )
    ),
    column(6,
      plotOutput("plot1")
    ),
    column(2,
      tableOutput("table"),
      downloadButton('downloadData', 'Download table of results')
    )
  )
))
