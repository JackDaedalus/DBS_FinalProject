library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Data Table Example"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Tabs', tabName='tabs',
               menuSubItem('Tab 1', tabName='tab1'),
               menuSubItem('Tab 2', tabName='tab2')
      )
    )
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName='tab1',
              actionButton("import","Import"),
              br(),
              tags$div(tags$h3(tags$b(" Get Selected Row Values",align="middle",style="color: rgb(57,156,8)"))),
              br(),
              DT::dataTableOutput('ProductDataTable')
      ),
      tabItem(tabName='tab2',
              actionButton("display","Display"),
              uiOutput('info')
      )
    )
  )
)

server <- function(input, output) {
  
  Product <- reactive({
    if(input$import == 0)
    {
      return()
    }
    isolate({
      input$import
      data <- mtcars # Here read Your data: read.csv2("RulesData.csv", header=TRUE, sep=";")
    })
  })
  
  
  output$ProductDataTable <- DT::renderDataTable({
    
    DT::datatable(Product(),selection = "single",
                  
                  extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller'),
                  rownames=FALSE,
                  options=list(dom = 'Bfrtip',
                               searching = T,
                               pageLength = 25,
                               searchHighlight = TRUE,
                               colReorder = TRUE,
                               fixedHeader = TRUE,
                               filter = 'bottom',
                               buttons = c('copy', 'csv','excel', 'print'),
                               paging    = TRUE,
                               deferRender = TRUE,
                               scroller = TRUE,
                               scrollX = TRUE,
                               scrollY = 700
                               
                  ))
  })
  
  
  observeEvent(input$display,{
    
    
    output$info <- renderPrint({
      row_count <- input$ProductDataTable_rows_selected
      data <- Product()[row_count, ] 
      cat('Row Selected: ')
      cat(data[,1]) #display the selected row 1st col value  
      
      
    })
    
  })
}

shinyApp(ui, server)