require(shiny)

ui <- shinyUI(fluidPage(    
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("rock", "pressure", "cars","DNase","iris")
      ),
      selectizeInput(
        'colName', 'Select Column: ', list(), multiple = TRUE
      ),
      selectizeInput(
        'rowName', 'Select Rows', list(), multiple = TRUE
      )
    ),
    mainPanel(
      tableOutput('tbl')
    )
  ) #end sidebar layout
))

server <- shinyServer(function(input, output, session) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars,
           "DNase"=DNase,
           "iris"=iris)
  })
  
  # Update UI
  observe({
    updateSelectizeInput(session, "colName", choices = colnames( datasetInput() ))  
    updateSelectizeInput(session, "rowName", choices = rownames( datasetInput() ))  
  })
  
  # Create reactive data by subseting the reactive dataset
  r1 <- reactive({
    v <- input$colName %in% colnames(datasetInput())
    if( sum(v == FALSE) > 0) return() # Check for missmatching datasetInput names and column names
    if(is.null(input$colName) || is.null(input$rowName)) return() # None selected, return empty
    
    # Subset data
    datasetInput()[as.numeric(input$rowName), input$colName, drop=FALSE]
  })
  
  output$tbl <- renderTable({ 
    r1()
  })
})

shinyApp(ui, server)