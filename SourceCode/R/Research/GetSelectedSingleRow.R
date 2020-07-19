# global.R 

library(DT)
library(shiny)
selectedrowindex = 0

#The below code display a dataframe in DT table format. Users will be able to select single row. 
#The selected row is retrieved and displayed. you can write your plot function in the plot block in the server.
#I hope this helps !!
  
  # Server.R
  shinyServer(function(input, output,session) {
    
    
    
    
    output$sampletable <- DT::renderDataTable({
      sampletable
    }, server = TRUE,selection = 'single')  
    
    output$selectedrow <- DT::renderDataTable({
      
      selectedrowindex <<-     input$sampletable_rows_selected[length(input$sampletable_rows_selected)]
      selectedrowindex <<- as.numeric(selectedrowindex)
      selectedrow <- (sampletable[selectedrowindex,])
      selectedrow
      
      
      
    })
    
    output$plots <- renderPlot({
      
      variable <- sampletable[selectedrowindex,1]
      #write your plot function
      
      
    })
    
    
  })

#ui.R 
shinyUI(navbarPage( "Single Row Selection",
                    
                    
                    
                    tabPanel("Row selection example",
                             sidebarLayout(sidebarPanel("Parameters"),
                                           mainPanel(
                                             DT::dataTableOutput("selectedrow"),   
                                             DT::dataTableOutput("sampletable")
                                             
                                           ))
                             
                    )
                    
))


# Create Shiny app ----
shinyApp(shinyUI, shinyServer)