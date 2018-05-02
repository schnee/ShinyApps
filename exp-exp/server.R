library(shiny)
library(gtools)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(broom)
library(tidyr)
library(fitdistrplus)


source("./prob-b-beats-a.R")

# q>p, compute the probability of a
# p-rate process measuring as q-rate
# or better in n steps
pSignificanceError <- function(p, q, n) {
  pbinom(
    ceiling(q * n) - 1,
    prob = p,
    size = n,
    lower.tail = FALSE
  )
}

# q>p, compute the proability of a
# q-rate process measuring as p-rate
# or lower in n steps
pPowerError <- function(p, q, n) {
  pbinom(floor(p * n),
         prob = q,
         size = n,
         lower.tail = TRUE)
}

designExperiment <- function(pA,
                             pB,
                             pError,
                             pAUpper = pB,
                             pBLower = pA) {
  aSoln <- binsearch(function(k) {
    pSignificanceError(pA, pAUpper, k) - pError
  },
  range = c(100, 1000000))
  nA <- max(aSoln$where)
  
  bSoln <- binsearch(function(k) {
    pPowerError(pBLower, pB, k) - pError
  },
  range = c(100, 1000000))
  nB <- max(bSoln$where)
  
  low = floor(min(pA * nA, pB * nB))
  high = ceiling(max(pA * nA, pB * nB))
  width = high - low
  countRange <- (low - width):(high + width)
  
  dA <- data.frame(count = countRange)
  dA$group <- paste('A: sample size=', nA, sep = '')
  dA$density <- dbinom(dA$count, prob = pA, size = nA)
  dA$rate <- dA$count / nA
  dA$error <- dA$rate >= pAUpper
  dB <- data.frame(count = countRange)
  dB$group <- paste('B: sample size=', nB, sep = '')
  dB$density <- dbinom(dB$count, prob = pB, size = nB)
  dB$rate <- dB$count / nB
  dB$error <- dB$rate <= pBLower
  d <- rbind(dA, dB)
  
  plot = ggplot(data = d, aes(x = rate, y = density)) +
    geom_line() +
    geom_ribbon(
      data = subset(d, error),
      aes(ymin = 0, ymax = density),
      fill = scales::brewer_pal(palette = "Set1")(1)[1]
    ) +
    facet_wrap( ~ group, ncol = 1, scales = 'free_y') +
    geom_vline(xintercept = pAUpper, linetype = 2) +
    geom_vline(xintercept = pBLower, linetype = 2) +
    theme_few()
  list(nA = nA, nB = nB, plot = plot)
}

getPlot <- function (pA, pB, pError, concurrent) {
  if (pB > pA) {
    if (concurrent) {
      r <-
        suppressWarnings(designExperiment(pA, pB, pError, ave(c(pA, pB)), ave(c(pA, pB))))
    } else{
      r = suppressWarnings(designExperiment(pA, pB, pError))
    }
    r$plot
  }
}

shinyServer(function(input, output, session) {
  output$exp = renderPlot({
    input$abButton
    isolate({
      pA = input$pA
      pB = input$pB
    })
    
    validate(
      need(
        pA < pB,
        "Please ensure Current Conversion Rate is less than Desired Conversion Rate"
      )
    )
    getPlot(pA, pB, input$pError, input$concurrent)
  })
  
  #   output$o_text = renderText({
  #     input$probButton
  #
  #     isolate({
  #       a_s = input$successes_a
  #       a_v = input$visits_a
  #       b_s = input$successes_b
  #       b_v = input$visits_b
  #
  #       # calculate the values at the Observed point
  #
  #       alpha_a = a_s + 1
  #       beta_a = a_v - a_s +1
  #       alpha_b = b_s +1
  #       beta_b = b_v - b_s + 1
  #
  #       prob <- round(prob_B_beats_A(alpha_a, beta_a, alpha_b, beta_b),
  #                     digits=4)
  #
  #       paste("At ", b_v, " observations the probability is ",prob)
  #     })
  
  probFn <- reactive({
    input$probButton
    isolate({
      a_s = input$successes_a
      a_v = input$visits_a
      b_s = input$successes_b
      b_v = input$visits_b
      
      
      alpha_a = a_s + 1
      beta_a = a_v - a_s + 1
      alpha_b = b_s + 1
      beta_b = b_v - b_s + 1
      
      round(prob_B_beats_A(alpha_a, beta_a, alpha_b, beta_b),
            digits = 4)
    })
    
  })
  
  output$o_text = renderText({
    paste("At ",
          input$visits_b,
          " trials the probability of B beating A is ",
          probFn())
  })
  
  df_ebFN <- reactive({
    input$probButton
    isolate({
      get_bayes_density_df(input$successes_a,
                           input$visits_a,
                           input$successes_b,
                           input$visits_b)
    })
  })
  
  minmax_dfFN <- reactive({
      get_limits_df(df_ebFN())
  })
  
  output$bayesPlot = renderPlot({
    df_eb <- df_ebFN()

    minmax_df <- minmax_dfFN()
    
    bayesian_density_plot2(df_eb, minmax_df)

  })
  
  output$bayesJointPlot <- renderPlot({
    bayesian_joint_plot(df_ebFN(), minmax_dfFN())
  })
})
