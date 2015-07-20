# User interface for simulation-based statistical power calculator app
# For starters, pulled UI code from https://github.com/rstudio/shiny-examples/blob/master/051-movie-explorer/ui.R
library(shiny)
library(shinyapps)
shinyUI(fluidPage(
  titlePanel("Experimental Design Power Calculator"), # Simulation-Based Statistical 
{ 
  wellPanel(
    fluidRow(
      column(3, 
        h4("Study Design"),
        radioButtons(inputId = "design",
                     label = NULL,
                     choices = c("RCT/Difference Between Groups",
                                 "Single Population Estimate (inactive)",
                                 "LQAS (inactive)"), 
                     selected = NULL,
                     inline = FALSE),
        checkboxInput(inputId = "clusterDesign",
                      label = "Clustered study design?",
                      value = TRUE)
      ),
      column(3,
        h4("Outcome Type"),
        radioButtons(inputId = "outcomeType",
                     label = NULL,
                     choices = c("Continuous",
                                 "Binary (inactive)",
                                 "Count (inactive)"),
                     selected = NULL,
                     inline = FALSE)
      ),
      column(3,
        h4("Longitudinal Design (inactive)"),
        checkboxInput("longDesign", "Longitudinal study design?", value = F),
        conditionalPanel(
          condition = "input.longDesign == true",
          numericInput("long.followups", "Number of follow-ups", 1, min = 1, max = 10),
          sliderInput("long.ICC", "ICC for longitudinal measures", min = 0.0, max = 1.0, step = 0.01, value = 0.25)
        )
      )
    )
  )
}, # This is the header control panel
  #br(), hr(), br(),
  fluidRow(
    column(3,
      wellPanel(
        p(actionButton("updateCalc", "Update calculation", icon("bolt"))), # look here for all icons - http://fontawesome.io/icons/
        h4("Specify Request"),
        conditionalPanel(condition = "input.clusterDesign == true",
          selectInput(inputId = "clusterRequest",
                      label = "What relationship do you need to explore?",
                      choices = c("power vs. effect size",
                                  "effect size vs. cluster size",
                                  "effect size vs. # clusters"), # "cluster size vs. # clusters", ... not yet working
                      selected = "power vs. effect size"),
          conditionalPanel(condition = "input.clusterRequest == 'cluster size vs. # clusters'",
            numericInput(inputId = "power_sn",
                         label   = "Power",
                         value = 0.8,
                         min = 0.0,
                         max = 1.0),
            numericInput(inputId = "effect.size_sn",
                         label = "Effect size",
                         value = 0.5,
                         min = 0,
                         max = 4)
          ),
          conditionalPanel(condition = "input.clusterRequest == 'power vs. effect size'",
            numericInput(inputId = "cluster.size_pe",
                         label = "Sample size per cluster",
                         value = 50,
                         min = 1,
                         max = 1000),
            numericInput(inputId = "cluster.num_pe",
                         label = "Number of clusters",
                         value = 20,
                         min = 1,
                         max = 100)
          ),
          conditionalPanel(condition = "input.clusterRequest == 'effect size vs. cluster size'",
            numericInput(inputId = "power_es",
                         label = "Power",
                         value = 0.8,
                         min = 0.0,
                         max = 1.0),
            numericInput(inputId = "cluster.num_es",
                         label = "Number of clusters",
                         value = 20,
                         min = 1,
                         max = 100)
          ),
          conditionalPanel(condition = "input.clusterRequest == 'effect size vs. # clusters'",
            numericInput(inputId = "power_en",
                         label = "Power",
                         value = 0.8,
                         min = 0.0,
                         max = 1.0),
            numericInput(inputId = "cluster.size_en",
                         label = "Sample size per cluster",
                         value = 50,
                         min = 1,
                         max = 1000)
          )
        ),
        conditionalPanel(condition = "input.clusterDesign == false",
          selectInput(inputId = "indvRequest",
                      label = "What relationship do you need to explore?",
                      choices = c("power vs. effect size",
                                  "effect size vs. sample size"), # "power vs. sample size", ... isn't working yet
                      selected = "power vs. effect size"),
          conditionalPanel(condition = "input.indvRequest == 'power vs. sample size'",
            numericInput(inputId = "effect.size_psIndv",
                         label = "Effect size",
                         value = 0.5,
                         min = 0,
                         max = 4)
          ),
          conditionalPanel(condition = "input.indvRequest == 'power vs. effect size'",
            numericInput(inputId = "sample.size_peIndv",
                         label = "Sample size",
                         value = 50,
                         min = 1,
                         max = 1000)
          ),
          conditionalPanel(condition = "input.indvRequest == 'effect size vs. sample size'",
            numericInput(inputId = "power_esIndv",
                         label = "Power",
                         value = 0.8,
                         min = 0.0,
                         max = 1.0)
          )
        ),
        numericInput(inputId = "resid.sd",
                     label = "Standard deviation of the outcome",
                     value = 1.0,
                     min = 0.01,
                     max = 10.0)
      )
    ),
    column(3,
      wellPanel(
        h4("Power parameters"),
        sliderInput("alpha", HTML("Alpha (&alpha;)"), 0.05, min = 0.01, max = 0.1, step = 0.01),
        #tags$div(title = withMathJax("The equation for ICC is: $$\\frac{\\sigma^2_{Between}}{\\sigma^2_{Total}}$$"), # 
        conditionalPanel(condition = "input.clusterDesign == true",
          sliderInput("cluster.ICC", "", label = "Intra-Cluster Correlation (ICC)", min = 0, max = 1, step = 0.01, value = 0.25),
          p("ICC is the fraction of the total individual variance that is attributable to between-cluster variance. An ICC=0
            approximates an individually randomized controlled study. The larger the ICC is, the more important cluster
            differences are in individual outcomes.")
            #withMathJax("The equation for ICC is: $$\\frac{\\sigma^2_{Between}}{\\sigma^2_{Total}}$$")
        )
      )
      
#       wellPanel(
#         h4("Treatment Parameters"),
#         sliderInput("baseline.prev", "Prevalence under control conditions",   min = 0, max = 1, step = 0.01, value = 0.8),
#         selectInput("trtSpec", "Method of entering treatment effect", c("Prevalence", "Odds Ratio")),
#         conditionalPanel(
#           condition = "input.trtSpec == 'Prevalence'",
#           sliderInput("trt.prev", "Prevalence under treatment conditions", min = 0, max = 1, step = 0.01, value = c(0.6, 0.8)),
#           numericInput("trt.vals.num",  "Number of values to test in this range", 5, min = 0, max = 100),
#           p("Prevalence under control conditions can be estimated using either baseline values pilot data or the literature.
#             Prevalence number treatment is the estimated minimum prevalence you want to detect.")
#         ),
#         conditionalPanel(
#           condition = "input.trtSpec == 'Odds Ratio'",
#           sliderInput("or.list", "Odds ratio", min = 0, max = 1, step = 0.01, value = c(0.75, 1.00)),
#           numericInput("or.vals.num",  "Number of values to test in this range", 5, min = 0, max = 100), 
#           p("The odds ratio represents the treatment effect--relative to baseline--that you want to detect.") #, where values below zero represent a protective effect, and those above zero represent detrimental effects.
#         )
#       )
    ),
    column(6,
      plotOutput("plotOut")
    )
#     ,column(2,
#       tableOutput("table"),
#       downloadButton('downloadData', 'Download table of results'),
#       textOutput("cores"),
#       textOutput("iter")
#     )
  )
))
