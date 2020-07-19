library(shiny)
library(DT)
ui <- function(id) {
  fluidPage(
    title = "Job Tracker",
    
    fluidRow(
      column(width=6,
             selectInput("pickvalue", label = "Pick a Value", choices = unique(iris$Species))
      )
    ),
    
    br(),
    fluidRow(
      column(12,
             DT::dataTableOutput("job_data")
             
      )
      
    ),
    br(),
    fluidRow(
      column(12,DT::dataTableOutput("x4"))
    )
  )
  
}

server <- function(input, output, session){
  v <- reactiveValues()
  v$s <- NULL
  
  data <- reactive({
    iris[iris$Species==input$pickvalue,]
  })
  
  output$job_data  <- DT::renderDataTable({
    datatable(data(),selection = "single")
  })
  
  observe({
    if(!is.null(input$job_data_rows_selected)){
      v$s <- input$job_data_rows_selected
    }
  })
  
  output$x4 <- DT::renderDataTable({
    datatable(data()[v$s,])
  })
}
shinyApp(ui, server)