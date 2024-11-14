# Load required libraries
library(shiny)
library(class) # For KNN

# Load the KNN model data
# Make sure to have saved the training data and k value to knn_model_data.rds first
knn_setup <- readRDS("knn_model_data.rds")
train_data <- knn_setup$train_data
k <- knn_setup$k

# Define UI
ui <- fluidPage(
  titlePanel("Bankruptcy Prediction using K-Nearest Neighbors (KNN)"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Input Values for Prediction"),
      
      # Input fields for each feature in the dataset
      numericInput("ROA_C", "ROA(C) before interest and depreciation before interest", value = 0),
      numericInput("ROA_A", "ROA(A) before interest and % after tax", value = 0),
      numericInput("ROA_B", "ROA(B) before interest and depreciation after tax", value = 0),
      numericInput("Operating_Gross_Margin", "Operating Gross Margin", value = 0),
      numericInput("Realized_Sales_Gross_Margin", "Realized Sales Gross Margin", value = 0),
      numericInput("Operating_Profit_Rate", "Operating Profit Rate", value = 0),
      numericInput("Pre_tax_net_Interest_Rate", "Pre-tax net Interest Rate", value = 0),
      numericInput("After_tax_net_Interest_Rate", "After-tax net Interest Rate", value = 0),
      numericInput("Non_industry_income_expenditure", "Non-industry income and expenditure/revenue", value = 0),
      numericInput("Continuous_interest_rate", "Continuous interest rate (after tax)", value = 0),
      numericInput("Operating_Expense_Rate", "Operating Expense Rate", value = 0),
      numericInput("Cash_flow_rate", "Cash flow rate", value = 0),
      numericInput("Interest_bearing_debt_interest_rate", "Interest-bearing debt interest rate", value = 0),
      
      actionButton("predict", "Predict Bankruptcy")
    ),
    
    mainPanel(
      h3("Prediction Result"),
      verbatimTextOutput("prediction_output")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict, {
    # Collect input values into a dataframe with the same structure as the training data (excluding target column)
    new_data <- data.frame(
      `ROA(C) before interest and depreciation before interest` = input$ROA_C,
      `ROA(A) before interest and % after tax` = input$ROA_A,
      `ROA(B) before interest and depreciation after tax` = input$ROA_B,
      `Operating Gross Margin` = input$Operating_Gross_Margin,
      `Realized Sales Gross Margin` = input$Realized_Sales_Gross_Margin,
      `Operating Profit Rate` = input$Operating_Profit_Rate,
      `Pre-tax net Interest Rate` = input$Pre_tax_net_Interest_Rate,
      `After-tax net Interest Rate` = input$After_tax_net_Interest_Rate,
      `Non-industry income and expenditure/revenue` = input$Non_industry_income_expenditure,
      `Continuous interest rate (after tax)` = input$Continuous_interest_rate,
      `Operating Expense Rate` = input$Operating_Expense_Rate,
      `Cash flow rate` = input$Cash_flow_rate,
      `Interest-bearing debt interest rate` = input$Interest_bearing_debt_interest_rate
    )
    
    # KNN prediction
    knn_prediction <- knn(
      train = train_data[, -ncol(train_data)],  # Use training data excluding the target column
      test = new_data,
      cl = train_data$Bankrupt.,  # Target column in training data
      k = k
    )
    
    # Display prediction result
    output$prediction_output <- renderText({
      paste("Predicted Class:", ifelse(knn_prediction == 1, "Bankrupt", "Not Bankrupt"))
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
