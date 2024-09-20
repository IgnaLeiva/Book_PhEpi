library(shiny)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Odds Ratio and Risk Ratio Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select OR or RR
      selectInput("measure", "Select Measure:", 
                  choices = list("Odds Ratio (OR)" = "OR", "Risk Ratio (RR)" = "RR")),
      
      # Input fields for A, B, C, and D
      numericInput("A_input", "A (e.g., Exposed cases):", value = 20),
      numericInput("B_input", "B (e.g., Exposed non-cases):", value = 80),
      numericInput("C_input", "C (e.g., Unexposed cases):", value = 15),
      numericInput("D_input", "D (e.g., Unexposed non-cases):", value = 85),
      
      # Dropdown to select CI level or enter custom
      numericInput("CI_level", "Confidence Level (%):", value = 95, min = 50, max = 99)
    ),
    
    mainPanel(
      # Output text for the selected measure and custom CI
      textOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to calculate OR or RR and custom CI
  output$result <- renderText({
    
    # Assign values from user inputs
    A <- input$A_input
    B <- input$B_input
    C <- input$C_input
    D <- input$D_input
    CI_level <- input$CI_level / 100  # Convert CI level to decimal
    
    # Calculate z-value based on CI level
    z_value <- qnorm(1 - (1 - CI_level) / 2)
    
    # Check whether to calculate OR or RR based on user input
    if (input$measure == "OR") {
      # Calculate Odds Ratio (OR)
      OR <- (A * D) / (B * C)
      
      # Calculate custom CI for OR
      lower_CI <- exp(log(OR) - z_value * sqrt(1/A + 1/B + 1/C + 1/D))
      upper_CI <- exp(log(OR) + z_value * sqrt(1/A + 1/B + 1/C + 1/D))
      
      # Display OR result
      paste0("Odds Ratio (OR): ", round(OR, 2), 
             ", ", round(input$CI_level, 0), "% CI: ", round(lower_CI, 2), " - ", round(upper_CI, 2))
      
    } else {
      # Calculate Risk Ratio (RR)
      risk_exposed <- A / (A + B)
      risk_unexposed <- C / (C + D)
      RR <- risk_exposed / risk_unexposed
      
      # Calculate custom CI for RR
      lower_CI <- exp(log(RR) - z_value * sqrt((1/A) - (1/(A+B)) + (1/C) - (1/(C+D))))
      upper_CI <- exp(log(RR) + z_value * sqrt((1/A) - (1/(A+B)) + (1/C) - (1/(C+D))))
      
      # Display RR result
      paste0("Risk Ratio (RR): ", round(RR, 2), 
             ", ", round(input$CI_level, 0), "% CI: ", round(lower_CI, 2), " - ", round(upper_CI, 2))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)