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

library(shiny)

shinyUI(fluidPage(
  title = "Bayesian Estimation of Change on the AVLT",
  # fluidRow(
  #   column(8,
  #          h2("Bayesian Estimation of Change on the AVLT"),
  #          HTML('If this app is valuable to you, please consider making a small donation to help support hosting fees.
  #    <a href = "https://www.paypal.me/begavett/"><img src = "https://www.paypalobjects.com/webstatic/en_US/i/btn/png/btn_donate_74x21.png" /></a>')),
  #   column(4,
  #          img(src = "http://www.uccs.edu/Images/brand/uccs-logo.png", width=400, height=58))),

  #Sidebar with controls to enter data
  sidebarLayout(sidebarPanel(
    numericInput("age", "Age at baseline (years):",70,min=51,max=91),
    numericInput("edu", "Education (years):",12,min=4,max=20),
    radioButtons("gender", "Gender:", choices = c("Female", "Male"), selected = "Female"),
    radioButtons("race", "Race:", choices = c("White/Caucasian", "Non White/Caucasian"), selected = "White/Caucasian"),
    radioButtons("ethnic", "Ethnicity:", choices = c("Hispanic/Latino", "Not Hispanic/Latino"), selected = "Not Hispanic/Latino"),
    radioButtons("marital", "Marital Status:", choices = c("Married", "Widowed", "Divorced", "Never married"), selected = "Married"),
    numericInput("interval", "Test-Retest Interval (mos):",6,min=0,max=132),
    numericInput("nvisits", "Visit Number:",2,min=2,max=19),
    numericInput("amnart", "Baseline Premorbid IQ (AMNART):", 100, 82, 129),
    radioButtons("bldx", "Baseline Diagnosis:", choices = c("Control", "MCI", "AD"), selected = "Control"),
    numericInput("avlt", "Baseline AVLT Total 1-5 Score:", 30, 0, 75),
    numericInput("fuavlt", "Follow-up AVLT Total 1-5 Score:", 25, 0, 75),
  submitButton("Calculate")),

  mainPanel(h4("Bayesian Estimate of Change"),
            plotOutput("Plot"),
            textOutput("Summary"))
)))
