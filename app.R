library(shiny)

ui <- fluidPage(
  tags$h3("Gradient Descent with a fixed step-size: 
          A peek under the hood using Linear Regression"),
  tags$h4("- Adjust the sliders to fit the red regression 
          line on Chart 4, using the gradient descent algorithm"),
  tags$h4("- Use the Resample button to generate a new set of 100 random variables"
  ),
  tags$br(),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "step.size", 
                  label = "Adjust Step Size", 
                  value = 1e-6, min = 1e-6, max = 1e-4, step = 1e-6),
      sliderInput(inputId = "max.iterations", 
                  label = "Adjust number of iterations", 
                  value = 100, min = 100, max = 1000, step = 100),
      actionButton("renorm", "Resample")
    ),
    mainPanel(
      plotOutput("gradient.descent")
    )
  )
)

server <- function(input, output) {
  n = 100
  rv <- reactiveValues(norm1 = rnorm(n),
                       norm2 = rnorm(n))
  
  observeEvent(input$renorm, { rv$norm1 <- rnorm(n);  rv$norm2 <- rnorm(n)})
  
  output$gradient.descent <- renderPlot({
    
   # set.seed(400)
    x = rv$norm1; x2 = rv$norm2
    
    ## Generate linearly realted data
    y = 1 + 2 * x + + x2 + rnorm(n, sd = .5)
    
    # generate a model with built-in lm
    fit.lm <- lm(y ~ x)
    
    # initialize algorith parameters
    step_size <- input$step.size
    init_intercept <- 0
    init_slope <- 0
    magnitude <- 0
    converged <- FALSE
    
    w0 <- init_intercept
    w1 <- init_slope
    
    n <- 0
    iterations <- input$max.iterations
    
    # keep history
    w0_history <- double(iterations)
    w1_history <- double(iterations)
    mag_history <- double(iterations)
    res_history <- double(iterations)
    
    # In each step of the gradient descent do the following:
    
    while(!converged){
      # Compute the predicted values given the current slope and intercept
      yhat <- w0 + w1 * x 
      
      # Compute the prediction errors (prediction - Y)
      err_predict <- yhat - y

      # sum of squared prediction error
      sqrd_err <- sum(err_predict^2)
    
      # Update the intercept:
      
      # compute the derivative: sum(errors)
      w0.derivative <- sum(err_predict)
      
      # compute the adjustment as, step_size times the derivative
      w0.adjustment <- step_size * w0.derivative
      
      # decrease the intercept by the adjustment
      w0 <- w0 - w0.adjustment
      
      # Update the slope:
      
      # compute the derivative: sum(errors*input)
      w1.derivative <- sum(err_predict * x)
      
      # compute the adjustment as, step_size times the derivative
      w1.adjustment <- step_size * w1.derivative
      
      # decrease the slope by the adjustment
      w1 <- w1 - w1.adjustment
      
      # Compute the magnitude of the gradient
      magnitude <- sqrt(w0.derivative^2 + w1.derivative^2)
      
      # update iteration counter
      n <- n + 1
      
      # update the history
      w0_history[n] <- w0
      w1_history[n] <- w1
      mag_history[n] <- magnitude
      res_history[n] <- sqrd_err
      
      if(n == iterations) break
      
    }
    
    # configure canvas for plotting
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
    
    # plot the data
    plot(w0_history, type='line', col='orange', lwd=2, ylab='Intercept', xlab='Iterations', 
         main = "Chart 1: The Intercept")
    plot(w1_history, type='line', col='green', lwd=2, ylab='Slope', xlab='Iterations',
         main = "Chart 2: The Slope")
    plot(res_history, type='line', col='blue', lwd=2, ylab='prediction error', xlab='Iterations',
         main = "Chart 3: The sum of squared prediction errors")
    
    # plot scatter plot for x and y variables
    plot(x, y, main = "Chart 4: Fitted Regression lines: lm (black), gradient descent(red)")
    
    # plot regression line returned by built-in lm model
    abline(a = fit.lm$coefficients[1], b = fit.lm$coefficients[2])
   
    # plot regession line given by current intercept and slope
    abline(a = w0, b = w1, col = "red")
  })
}

shinyApp(ui = ui, server = server)




