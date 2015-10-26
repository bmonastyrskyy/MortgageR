# Author : Bohdan Monastyrskyy
# Date : 2015-10-26
# Description : shiny web-interface of MortgageR project


library("shiny")

source("Functions.R")

ui <- fluidPage(
  headerPanel("Mortgage Calculator"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = 'loanType', label = h3("Type of Loan"),
                   choices = list("Fixed" = 'fixed', "Adjusted" = 'adjusted'),
                   selected = 'fixed'),
      numericInput(inputId = "loan", "Loan", value = 250000, step = 5000),
      numericInput(inputId = "period", "Period (years)", value = 30, step = 1),
      textInput(inputId = "APR", "APR", value = "4.0"),
      numericInput(inputId = "payment", "Payment", value = NA),
      textInput(inputId = "breaks", label = "Breaks", value = NA),
      submitButton("Update")
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
  re.dfm <- reactive({ switch (input$loanType,
                               'fixed' =
                        mortgagePaymentFixed(loan = input$loan, APR = input$APR, period = (input$period * 12),
                                          payment = ifelse(is.null(input$payment) | is.na(input$payment), NA, input$payment)),
                                'adjusted' =
                        mortgagePaymentAdjusted(loan = input$loan, APR = input$APR, period = (input$period * 12), breaks = input$breaks,
                                                payment = ifelse(is.null(input$payment) | is.na(input$payment), NA, input$payment))
                        )
                      })

  output$summary <- renderPrint({
    loan.summary(re.dfm());
  })

  output$table <- renderTable(re.dfm())
  output$payment <- renderPlot(plotPayment(re.dfm()))
  output$amort <- renderPlot(plotAmortization(re.dfm()))
}

shinyApp(ui = ui, server = server)
