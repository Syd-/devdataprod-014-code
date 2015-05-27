library(shiny)
generateIndex <<- 0
shinyServer(
  function (input, output) {
    # This function runs the rexp for a number of iterations
    # Storing the means for each one of them.
    getMeans <- function(iterations, sampleSize, lambda) {
      simulatedMean <- rep(0, iterations)
      for (i in 1 : iterations) {
        simulatedMean[i] <- mean(rexp(sampleSize, rate = lambda))
      }
      return(simulatedMean)
    }
    
    # Make input reactive
    lambda <- reactive({input$lambda})
    sampleSize <- reactive({input$sampleSize})
    iterations <- reactive({input$iterations})

    # Calculate theoretical mean and sd
    output$mean <- renderText({1 / lambda()})
    output$standardDeviation <- renderText({((1 / lambda()) / sqrt(sampleSize()))})

    # Call function
    sampleMeans <- reactive({getMeans(iterations(), sampleSize(), lambda())})
    
    # Calculate actual mean and sd
    output$sampleMean <- renderText(mean(sampleMeans()))
    output$sampleStandardDeviation <- renderText(sd(sampleMeans()))
    
    # Calculate ideal histogram
    idealXAxis <- reactive({seq(min(sampleMeans()), max(sampleMeans()), length=100)})
    idealYAxis <- reactive({dnorm(idealXAxis(), mean=1/lambda(), sd=1/(lambda() * sqrt(sampleSize())))})
    
    # Plot histogram
    output$histogram <- renderPlot({
      hist(
        sampleMeans(),
        breaks = 100,
        probability = TRUE,
        xlab = "Mean",
        main = "Distribution of sample vs ideal means",
        ylab = "Density")
      abline(v = mean(sampleMeans()), col="purple")
      abline(v = 1/lambda(), col="red")
      lines(density(sampleMeans()), col="purple")
      lines(idealXAxis(), idealYAxis(), col="red")       
      legend('topright', c("sample", "ideal"), lwd=1, col=c("purple", "red"))
    })
  }
)