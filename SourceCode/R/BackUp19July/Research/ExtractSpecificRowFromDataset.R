library(shiny)

betReport <- read.csv("Sample_CC_Fraud_Txns.csv")


shinyServer(function(input, output) {
  observeEvent(input$submit, {
    
    output$x1 <- DT::renderDataTable(betReport)
    
    # print the selected indices                                                                                                           
    output$x2 = renderPrint({
      s = input$ID_rows_selected
      if (length(s)) {
        cat("This is your balance.")
        cat(s, sep = ",")
      } else {
        print("I shouldn't be here but the rows_selected is wrong")
      }
    })
  })
  
})



shinyUI(fluidPage(
  titlePanel("Bet Rewards Calculator"),
  
  fluidRow(
    column(3,
           selectizeInput("ID", choices = "ID",
                          label = h4("Rewards ID")),
           br(),
           actionButton("submit", "Submit")),
    
    column(7, offset = 2,
           DT::dataTableOutput("x1"),
           br(),
           textOutput("x2")
    )
  )
  
  
  
)
)


# Create Shiny app ----
shinyApp(shinyUI, shinyServer)