library(ggplot2)
library(grid)
library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("Visualizing Process Capability"),
    sidebarPanel(
        helpText("The purpose of this app is to provide a visualization of",
                 "process capability modeled as a standard normal distribution.",
                 "Organizations measure the capability of their processes",
                 "in order to understand the true quality levels of their products",
                 "and services."),
        sliderInput("sigma", 
                    label = "Choose the sigma level to display",
                    value = 3, min = 1, max = 6, step = 0.5),
        helpText("Vary the standard deviation to see how the outputs",
                 "are affected by increased process variability."),
        sliderInput('sd', 'Set the standard deviation', 
                    value = 1, min = 1, max = 3, step = 0.25),
        helpText("The graph on the right will use those inputs to calculate",
                 "the percentage of expected good products and the number",
                 "of defects per million opportunities."),
        helpText("Additional information about this add can be found at",
                 "this link:",
                 "http://razolock.github.io/DDP_slidify/index#1")),
    mainPanel(textOutput('selectedSigma'), 
              plotOutput('myPlot'),
              textOutput('percentGood'),
              textOutput('defectivePpm'))
))