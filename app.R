library("shiny")

source("Functions.R")

ui <- fluidPage(
  headerPanel("Mortgage Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "loan", "Loan", value = 250000, step = 5000),
      numericInput(inputId = "period", "Period (years)", value = 30, step = 1),
      numericInput(inputId = "APR", "APR", value = 4.0, step = 0.05),
      numericInput(inputId = "payment", "Payment", value = NA)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput(outputId = "summary")),
        tabPanel("Table", tableOutput(outputId = "table")),
        tabPanel("Payment Plot", plotOutput(outputId = "payment")),
        tabPanel("Amortization Plot", plotOutput(outputId = "amort"))
      )
    )
  )
)

server <- function(input, output){
  re.dfm <- reactive(mortgagePaymentFixed(loan = input$loan, APR = input$APR, period = (input$period * 12),
                                          payment = ifelse(is.null(input$payment) | is.na(input$payment), NA, input$payment)))
  output$summary <- renderPrint({
    loan.summary(re.dfm())
  })
  output$table <- renderTable(re.dfm())
  output$payment <- renderPlot(plotPayment(re.dfm()))
  output$amort <- renderPlot(plotAmortization(re.dfm()))
}

shinyApp(ui = ui, server = server)
