# The MIT License (MIT)
#
# Copyright (c) 2017 Brandon Gavett
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(brms)
library(ggplot2)
library(ggthemes)
library(LearnBayes)

# Define server logic required to generate estimates
shinyServer(function(input, output) {

  Lik <- function(obs, mu) prod(dnorm(obs, mu, sigmaStan))
  vecLik <- function(obs, mu) sapply(mu, Lik, obs = obs)
  avltStan <- readRDS("avltStan.RDS")
  sigmaStan <- VarCorr(avltStan)$residual__$sd[[1]] # for BRMS. Use sigma(avltStan) for rstanarm

  Prior <- reactive({
    whiteRace <- ifelse(input$race == "White/Caucasian", 1, 0)
    whiteRace <- factor(whiteRace, levels = 0:1, labels = c("No", "Yes"))
    ethnic <- factor(input$ethnic, levels = c("Hispanic/Latino", "Not Hispanic/Latino"), labels = c("Hisp/Latino", "Not Hisp/Latino"))

    inputDat <- data.frame(RID = -9999, Interval = input$interval, Age = input$age, Gender = input$gender, Edu = input$edu,
                           White = whiteRace, Eth = ethnic, Marry = input$marital, AMNART.bl = input$amnart,
                           DX.bl = input$bldx, BlScore = input$avlt, nVisits = input$nvisits)
    inputPP <- posterior_predict(avltStan, newdata = inputDat, re.form = NA, allow_new_levels = TRUE)
    return(inputPP)
  })

  # Likelihood <- reactive({
  #   mu_grid <- density(Prior())$x
  #   likGrid <- vecLik(input$fuavlt - input$avlt, mu_grid)
  #   return(likGrid)
  # })
  #
  # Posterior <- reactive({
  #   mu_grid <- density(Prior())$x
  #   prior_mu_grid <- density(Prior())$y
  #   delta <- mu_grid[2] - mu_grid[1]
  #   NC <- sum(vecLik(input$fuavlt - input$avlt, mu_grid) * prior_mu_grid * delta)
  #   post_mu <- vecLik(input$fuavlt - input$avlt, mu_grid) * prior_mu_grid/NC
  #   return(post_mu)
  # })

  output$Plot <- renderPlot({
    mu_grid <- density(Prior())$x
    prior_mu_grid <- density(Prior())$y
    delta <- mu_grid[2] - mu_grid[1]
    NC <- sum(vecLik(input$fuavlt - input$avlt, mu_grid) * prior_mu_grid * delta)
    likGrid <- vecLik(input$fuavlt - input$avlt, mu_grid)
    post_mu <- vecLik(input$fuavlt - input$avlt, mu_grid) * prior_mu_grid/NC

    muMdn <- median(discint(cbind(mu_grid, post_mu/sum(post_mu)), .999999)$set)
    muMaxP <- mu_grid[post_mu == max(post_mu)]
    l90 <- min(discint(cbind(mu_grid, post_mu/sum(post_mu)), .90)$set)
    u90 <- max(discint(cbind(mu_grid, post_mu/sum(post_mu)), .90)$set)

    pNeg <- sum(post_mu[mu_grid < 0])/sum(post_mu)
    pPos <- sum(post_mu[mu_grid >= 0])/sum(post_mu)

    pH1 <- post_mu[mu_grid == max(mu_grid[mu_grid <= input$fuavlt - input$avlt])]
    pH0 <- post_mu[mu_grid == max(mu_grid[mu_grid <= 0])]

    plotDat <- data.frame(change = rep(mu_grid, 3), Distribution = rep(c("Prior", "Posterior", "Likelihood"), each = length(mu_grid)),
                          Value = c(prior_mu_grid, post_mu, likGrid))
    plotDat$Distribution <- factor(plotDat$Distribution, levels = levels(plotDat$Distribution)[c(3, 1, 2)])

    BayesFig <- ggplot(plotDat, aes(x = change, y = Value, colour = Distribution, fill = Distribution)) +
      geom_line() + geom_ribbon(aes(ymin = 0, ymax = Value), alpha = .1) +
      theme_few(base_size = 12)  +
      scale_fill_manual(values = c("white", "white", "black")) +
      geom_rect(xmin = min(mu_grid), xmax = l90, ymin = -1, ymax = max(plotDat$Value + .1), colour = "white", fill = "white") +
      geom_rect(xmin = u90, xmax = max(mu_grid), ymin = -1, ymax = max(plotDat$Value + .1), colour = "white", fill = "white") +
      geom_line() + geom_segment(aes(x = l90, y = 0, xend = u90, yend = 0), colour = "lightgrey") +
      annotate(geom = "text", x = c(l90, muMdn, u90), y = c(.0025, .0025, .0025),
               label = paste(c(round(l90, 2), round(muMdn, 2), round(u90, 2))))  +
      annotate(geom = "text", x = -15, y = max(plotDat$Value),
               label = paste0("p(Change < 0 | X) = ", round(pNeg, 2))) +
      annotate(geom = "text", x = 15, y = max(plotDat$Value),
               label = paste0("p(Change >= 0 | X) = ", round(pPos, 2))) +
      annotate(geom = "text", x = muMaxP, y = max(post_mu) + .005,
               label = paste0("Max = ", round(muMaxP, 2))) +
      annotate(geom = "text", x = c(l90, muMdn, u90), y = c(-.0025, -.0025, -.0025),
               label = c("HDI_Lo", "Mdn", "HDI_Hi")) + xlim(-20, 20) + ylim(-.0025, .125)
    return(BayesFig)
  })

  output$Summary <- renderText({
  mu_grid <- density(Prior())$x
  prior_mu_grid <- density(Prior())$y
  delta <- mu_grid[2] - mu_grid[1]
  NC <- sum(vecLik(input$fuavlt - input$avlt, mu_grid) * prior_mu_grid * delta)
  likGrid <- vecLik(input$fuavlt - input$avlt, mu_grid)
  post_mu <- vecLik(input$fuavlt - input$avlt, mu_grid) * prior_mu_grid/NC

  muMdn <- median(discint(cbind(mu_grid, post_mu/sum(post_mu)), .999999)$set)
  muMaxP <- mu_grid[post_mu == max(post_mu)]
  l90 <- min(discint(cbind(mu_grid, post_mu/sum(post_mu)), .90)$set)
  u90 <- max(discint(cbind(mu_grid, post_mu/sum(post_mu)), .90)$set)

  pNeg <- sum(post_mu[mu_grid < 0])/sum(post_mu)
  pPos <- sum(post_mu[mu_grid >= 0])/sum(post_mu)

  pH1 <- post_mu[mu_grid == max(mu_grid[mu_grid <= input$fuavlt - input$avlt])]
  pH0 <- post_mu[mu_grid == max(mu_grid[mu_grid <= 0])]
    writtenSummary <- paste0("The plot above shows the prior expectation for change (red line) based on the regression model applied to the baseline data entered on the left (all data except the follow-up test score). The likelihood function (green line) is based on the observed change of ", input$fuavlt - input$avlt, " points. The posterior distribution (blue line) represents the estimated distribution of possible values for true change that produced the observed change. Based on this posterior distribution, it is most likely that the examinee changed by ", round(muMaxP, 1), " points. The 90% highest density interval for true change (gray shading) ranges from ", round(l90, 1), " to ", round(u90, 1), ". There is a ", round(100*pNeg, 0),"% probability that the examinee truly declined and a ", round(100*pPos, 0), "% probability that the examinee truly improved. The evidence in favor of a ", input$fuavlt - input$avlt, "-point change is roughly ", ifelse(pH1/pH0 > 1, round(pH1/pH0, 1), round(pH0/pH1, 1)), " times ", ifelse(pH1/pH0 > 1, "greater", "less"), " than the evidence in favor of no change.")
  })
})
