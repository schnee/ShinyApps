suppressPackageStartupMessages(library(shiny))
#suppressPackageStartupMessages(library(ggvis))
suppressPackageStartupMessages(library(ggplot2))

# ui.R
shinyUI(navbarPage(
  "The Un-Official ABn Experiment Explorer",
  tabPanel("Design",
           sidebarLayout(
             sidebarPanel(
               a(href="http://www.win-vector.com/blog/2014/05/a-clear-picture-of-power-and-significance-in-ab-tests/",
                 "See here for details"),
               helpText("Very little error checking - keep the rates > 0 and < 1, and",
                        "keep the Current less than the Desired.",
                        "Even if you do all that, you may get the system into a",
                        "state that takes a long time to converge (if ever)."),
               numericInput("pA", "Current Conversion Rate", value=0.05, min=0, max=1),
               numericInput("pB", "Desired Conversion Rate", value=0.06, min=0, max=1),
               br(),
               actionButton("abButton", "Simulate"),
               br(),br(),br(),
               sliderInput("pError", label = "Error Rate",
                           min = 0.01, max = .10, value = 0.05, step=0.01),
               checkboxInput("concurrent", "Run Tests Concurrently", value=TRUE)
             ),
             mainPanel(
               plotOutput("exp")
             )
           )
  ),
  tabPanel("Probability", 
           sidebarLayout(
             sidebarPanel(
               a(href="http://www.evanmiller.org/bayesian-ab-testing.html", "See here for details"),
               helpText("Calculates the long-term Bayesian probability that the Variant would beat",
                        "the Control"),
               numericInput("successes_a", value=700,"Control Successes"),
               numericInput("visits_a", value=10000,"Control Trials"),
               numericInput("successes_b", value=750,"Variant Successes"),
               numericInput("visits_b", value=10000,"Variant Trials"),
               br(),
               actionButton("probButton", "Estimate"),
               br(),
               helpText("The probability is close-form calculated. The estimates are numerically simulated and
                        change (slightly) with each press of the \"Estimate\" button")
               
             ),
             mainPanel(
               textOutput("o_text"),
               helpText("Below are the Bayesian estimates of the long-term conversion rates"),
               plotOutput("bayesPlot", height = "300px"),
               helpText("The Joint Probability Density Plot helps visualize the probability of the winning variant. If the probability cloud is centered on the black line, then the probability of either being the winner is 50%"),
               plotOutput("bayesJointPlot", height = "300px", width = "300px")
             )
           )
  ),
  tabPanel("Help",
           mainPanel(
             p("To use the experiment designer, enter the \"conversion rate\" for both the A and B variants, and then
               select the error rate you are willing to tolerate. If example, if you want to be 95% sure that A and B are 
               different populations, select an error rate of 0.05. Then \"simulate\". The top graph shows the distribution
              of the A variant and the bottom graph shows the distribution of the B variant. The numbers at the top of each 
               graph show the number of observations needed to have confidence in the distributions. The numbers for A and B
               differ because of the concepts of \"significance\" and \"power\" - read the link on the top of the designer
               page for more detail, but in practice, these numbers are close enough to not really matter."),
             p("To use the probability tab, AT ANY TIME during the test, enter the number of successes and trials for
               both the A and B variants. You will see the long-term estimate of the probability that the B variant will
               beat the A variant. The graph shows the long-term Bayesian estimates of the distribution of the conversion
               rates. It is instructive to enter numbers for other-than-50/50 experiments - what happens if there are 10x more
               A trials than B trials (as well as successes)"),
             a(href="https://github.wvrgroup.internal/bschneeman/ShinyApps/tree/master/exp-exp", "Github.wvrgroup"),
             br(),
             a(href="mailto:schneeman@gmail.com?subject=Experiment Explorer Help", "Brent Schneeman")
           )
  )
)
)
