library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Demonstrating the Central Limit Theorem"),
  sidebarPanel(
    sliderInput("lambda",
      label = "lambda",
      min = 0.01,
      max = 1,
      value = 0.2),
    sliderInput("sampleSize",
      label = "Size of Distribution",
      min = 1,
      max = 100,
      value = 10),
    sliderInput("iterations",
      label = "Number of Iterations",
      min = 1,
      max = 10000,
      value = 10),
    tags$hr(),
    strong("Expected Mean"),
    verbatimTextOutput("mean"),
    strong("Expected Standard Deviation"),
    verbatimTextOutput("standardDeviation"),
    tags$hr(),
    strong("Data Set Mean"),
    verbatimTextOutput("sampleMean"),
    strong("Data Set Standard Deviation"),
    verbatimTextOutput("sampleStandardDeviation"),
    tags$hr(),
    h4("Sid M")
  ),
  mainPanel(
    h3("Introduction"),
    p("The central limit theorem (CLT) states that the arithmetic mean of a sufficiently large number of iterates of independent random variables will be approximately normally distributed, regardless of the underlying distribution."),
    p("Here, we demonstrate CLT by generating exponential distributions (rexp) and taking their means. We then repeat the above process to show CLT."),
    tags$hr(),
    p("Experiment with the sliders to the left to emperically verify CLT. :)"),
    p("Note how the histogram becomes normal as you increase iterations, and distribution size."),
    tags$hr(),
    h4("Histogram"),
    plotOutput('histogram'),
    tags$hr(),
    h4(tags$a(href = "https://github.com/Syd-/devdataprod-014-code", "Github Link")),
    tags$hr(),
    h4("Notes"),
    p("- Every time you change a value on the sliders, it will recalculate the means."),
    p("- The data set mean will tend to 1 / lambda."),
    p("- The data set standard deviation will tend to ((1 / lambda) / sqrt(size of distribution)).")    
  )
))
