shinyUI(pageWithSidebar(
    headerPanel("Visualizing Quality"),
    sidebarPanel(
        selectInput("sigma", 
                    label = "Choose the sigma level to display",
                    choices = c(1, 2, 3, 4, 5, 6),
                    selected = 3),
        sliderInput('sd', 'Set the standard deviation', 
                    value = 1, min = 1, max = 3, step = 0.25)),
    mainPanel(textOutput('selectedSigma'), 
              plotOutput('myPlot'),
              textOutput('percentGood'),
              textOutput('defectivePpm'))
))