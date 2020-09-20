library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)   # for %>%
library(broom)
library(caret)
library(cvms)
require(PerformanceAnalytics)



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
              verbatimTextOutput('TestDataTable'),
              br(),
              plotOutput("cmfPlot"),
              br(),
              dataTableOutput('TestDataTableDT'),
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
  
  
  MtDataTbl <- reactive({
    if(input$import == 0)
    {
      return()
    }
    isolate({
      input$import
      datatbl <- table(mtcars$am, mtcars$vs)
      # datatbl_1 <- tibble(datatbl)
      # datatbl_2 <- tidy(datatbl_1)
      # myDT <- datatbl_2 %>% datatable %>% 
      #   formatStyle(
      #     columns = 1:2,
      #     backgroundColor = styleInterval( 
      #       cuts = c(-.01, 0), 
      #       values = c("red", "white", "green")
      #     )
      #   )
    })
  })
  
  
  output$TestDataTable <- renderPrint({
    
    tst <- table(mtcars$am, mtcars$vs)
    tst <- confusionMatrix(tst)
    tst
    
  })
  
   output$TestDataTableDT <- renderDataTable({
     
     data.frame(
       capture.output(
         confusionMatrix(iris$Species, sample(iris$Species))
       )
     )
     
   })  
   
   output$cmfPlot <- renderPlot({   #Replaced renderPrint with renderPlot
     
     # textplot(      #wrap textplot around capture.output
     #   capture.output(     #capture output of confusionMatrix in text format
     #     confusionMatrix(iris$Species, sample(iris$Species))
     #   )      #close capture.output
     # )    #close textplot
     
     tst <- table(mtcars$am, mtcars$vs)
     #tst <- confusionMatrix(tst)
     tst1 <- as.data.frame(tst)
     p<-plot_confusion_matrix(tst1, 
                              targets_col = "target", 
                              predictions_col = "prediction",
                              counts_col = "n")
     
   }) #close renderPlot  
  
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