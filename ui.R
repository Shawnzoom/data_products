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