library(shiny)
library(shinydashboard)
library(ggplot2)

# Input data
ecom <- data.frame(
  Months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  Visitors = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  Transactions = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  Items.Transactions = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5, 5.2, 5.3, 5.4, 5.5),
  Rating = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9),
  Ads = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  Sales = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)
# Regression model
model <- lm(Sales ~ Transactions + Ads, data = ecom)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Monthly Sales Volume Estimation"),
  dashboardSidebar(),
  dashboardBody(
    # First row in dashboard layout 
    fluidRow(
      box(
        # diagnostic plots
        title = "Diagnostic Plots",
        solidHeader = TRUE,
        status = "primary",
        plotOutput("diagnostic_plot")
      ),
      box(
        # regression input to predict the working hours
        title = "Monthly Sales Volume Prediction",
        solidHeader = TRUE,
        status = "success",
        numericInput("input_x1", "Number of Website Visitors per Month:", value = 205000),
        numericInput("input_x2", "Number of Monthly Transactions:", value = 11333),
        numericInput("input_x3", "Average Number of Items per Transactions:", value = 5, step = 0.1),
        numericInput("input_x4", "Customer Satisfactions Rating (scale 1-10):", value = 8.7, step = 0.1),
        numericInput("input_x5", "Number of Online Advertisements Run per Month:", value = 33750),
        actionButton("predict_button", "Predict")
      )
    ),
    # Second row in dashboard layout
    fluidRow(
      # Display both predicted value and model coefficients in the same row
      valueBoxOutput("predicted_sales", width = 6),
      h4("Model Coefficients:"),
      verbatimTextOutput("coefficients_output")  # Adjust width as needed
    )
  )
)

# Define server
server <- function(input, output) {
  # Initialize value boxes with "Initial Value"
  # This is supposed to be the scorecard that shows the predicted working hours based on the model.
  output$predicted_sales <- renderValueBox({
    valueBox(
      "Initial Value",
      'Thousands USD Predicted Sales Volume per Month',
      icon = icon("usd", lib = 'glyphicon'),
      color = "green"
    )
  })
  
  # Output diagnostic plots
  output$diagnostic_plot <- renderPlot({
    par(mfrow = c(2, 2))
    plot(model)
  })
  
  # Use observeEvent to trigger update on button click
  observeEvent(input$predict_button, {
    # Predict sales based on user input
    predicted_y <- predict(model, newdata = data.frame(
      Visitors = input$input_x1,
      Transactions = input$input_x2,
      Items.Transactions = input$input_x3,
      Rating = input$input_x4,
      Ads = input$input_x5
    ))
    
    # Update the value of sales scorecard after the user clicked the predict button
    output$predicted_sales <- renderValueBox({
      valueBox(
        round(predicted_y, 2),
        'Thousands USD Predicted Sales Volume per Month',
        icon = icon("usd", lib = 'glyphicon'),
        color = "green"
      )
    })
    
    # Output model coefficients within the prediction box
    output$coefficients_output <- renderPrint({
      coefficients(model)
    })
  })
}

# Run the app
shinyApp(ui, server)
