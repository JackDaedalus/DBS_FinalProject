## Higher Diploma in Science in Data Analytics : Project : Module Code B8IT110
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## August 2020

## Application Part One - R Shniy Application performing descriptive analytics on Credit Card Fraud dataset
## The visualisations are presented through a Shiny UI dashboard


## This Shiny R application answer uses many standard R libraries, including an open source R Shiny package for 
## a Semantic Dashboard. Details of this open source package can be found here :...
## https://appsilon.com/semantic-dashboard-new-open-source-r-shiny-package/

#########################
library(shiny)
library(shinydashboard)
library(shinythemes)
library(semantic.dashboard)
library(reshape2)
library(ggplot2)
library(reshape2)
library(scales)
library(plotly)
library(ggcorrplot)
library(dplyr)
library(DT)
## Final Project Libraries
library(modules)
library(curl)
library(httr)
library(rjson)
library(stats)
library(readr)
## DT Formatting
library(magrittr)
library(data.table)
#########################


####################################################################################################
#devtools::install_github("RevolutionAnalytics/AzureML")
library("AzureML")

# R Code for project split into modular files. Set up calls to functions in R files in /Azure sub folder
api_call <- modules::use("Azure")
ui_data <- modules::use("UserApp")
source("UserApp/UI_FileSelection.R")
source("UserApp/EDA_PlotDisplays.R")


####################################################################################################


####################################################################################################
# Read Azure Hosted Credit Card Fraud Dataset for Final Production version of the project
ds_cctxns = api_call$AZURE_CreditCardFraud_Download$download25KProductionCCTxns()


# Read Azure Hosted Credit Card Fraud Dataset for Geographical Analysis of Fraud in dataset
frdCntReport = api_call$AZURE_CreditCardFraud_Download$downloadDevCntryCdCCTxns()



## ###########################
## Label fields in dataset to aid presentation of 2-dimensional matrix graphs on UI
## ###########################
ds_cctxns$Fraud <- as.factor(ds_cctxns$Fraud)
levels(ds_cctxns$Fraud) <- c("Not Fraud", "Fraud")   # Fraud

ds_cctxns$PinIndicator <- as.factor(ds_cctxns$PinIndicator)
levels(ds_cctxns$PinIndicator) <- c("No Pin", "With Pin")  # Pin Indicator

ds_cctxns$ECommerceFlag <- as.factor(ds_cctxns$ECommerceFlag)
levels(ds_cctxns$ECommerceFlag) <- c("Not ECommerce Trxn", "ECommerce Trxn")  # ECommerce Flag

ds_cctxns$CustomerPresentIndicator <- as.factor(ds_cctxns$CustomerPresentIndicator)
levels(ds_cctxns$CustomerPresentIndicator) <- c("No Customer", "Unknown", "Customer Present")  # Customer Present Indicator

## ###########################



## Temporary local dataset for testing main Production Fraud Predictive Model
cc_Txns = ui_data$DATA_FileSelection$ReturnDSWithSelectedFeatures()
cc_Txns <- cc_Txns[FALSE,]



## Limit the view of columns in the single selected cc trxn on screen
## List the columns NOT to show on the selected cc transaction - this is a parameter later used to 
## control a DataTable display on screen
columns2hide <- ui_data$DATA_FileSelection$SelectColumsToHide(cc_Txns)

####################################################################################################



# Shiny UI Function

ui <- dashboardPage(
  
  
  dashboardHeader(color = "blue", title = "PROJECT : Data Analytics : B8IT110", inverted = TRUE),
  
  dashboardSidebar(
    size = "wide", color = "teal",
    sidebarMenu(
      menuItem(tabName = "dataviz", text = "PROJECT: Credit Card Fraud : Dataset Overview", icon = icon("tv")),
      menuItem(tabName = "fraud_interface", text = "Real Time Fraud Interface", icon = icon("save"))
  )),
  
  dashboardBody(
    tabItems(
      selected = 1,
      
      ########################################################################################################
      ## First Tab on Dashboard
      ##
      ## This tab contains the results of exploratory data analysis on key characteristics of my credit card
      ## fraud transaction dataset
      ##
      ########################################################################################################
      tabItem(
        tabName = "dataviz",
        fluidRow(h1("PROJECT : Higher Diploma in Science in Data Analytics : Module B8IT110 : March 2019 Intake (Fri/Sat)")),
        fluidRow(h1("Ciaran Finnegan : Student Number : 10524150")),
        fluidRow(
          # Heading for dataset ----
          h2(htmlOutput("text.1"))
        ),
        fluidRow(
          # Heading for pie chart ----
          htmlOutput("text.2")
        ),
        fluidRow(
          # Output : Pie chart of Fraud Outcomes
          box( 
            width = 7,
            title = "Pie Chart : Fraud Outcomes - Full Dataset",
            color = "blue",
            ribbon = TRUE,
            title_side = "top right",
            column(
              width = 7,
              plotlyOutput("plot1", height = 250)
            )
          )
        ),
        br(),
        br(),
        br(),
        fluidRow(
          htmlOutput("text.3"),
          box( 
            width = 16,
            title = "BoxPlots : Transaction Amount Values Against Fraud Result : NO Outliers", 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot2", height = 350)
            )
          )
        ),
        br(),
        br(),
        br(),
        fluidRow(
          htmlOutput("text.4"),
          box( 
            width = 16,
            title = "BoxPlots : Transaction Amount Values Against Fraud Result : WITH Outliers", 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot3", height = 350)
            )
          )
        ),
        fluidRow(
          htmlOutput("text.6"),
          box(
              width = 8,
              title = "Four Fold Plot: PIN Use vs Incidence of Fraud",
              color = "blue",
              ribbon = TRUE,
              title_side = "top right",
              column(
                8,
                plotOutput("plot4", height = 350)
              )
            ),
          box(
              width = 8,
              title = "Four Fold Plot: Relationship between Ecommerce CC Trxnz and Fraud",
              color = "blue",
              ribbon = TRUE,
              title_side = "top right",
              column(
                8,
                plotOutput("plot5", height = 350)
              )
          ),
        ),
        br(),
        br(),
        fluidRow(
          htmlOutput("text.7"),
          box(
            width = 16,
            title = "Table: Relationship of Fraud to Customer Presence at CC Transaction",
            color = "blue",
            ribbon = TRUE,
            title_side = "top right",
            column(
              16,
              dataTableOutput("custPresent", width = "100%", height = "auto")
            )
          )
        ),  
        br(),
        br(),
        br(),
        br(),
        fluidRow(
          htmlOutput("text.8"),
          box( 
            width = 16,
            title = "100% Stacked Bar Chart: Fraud Breakdown Per Region", 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot6", height = 350)
            )
          )
        )
      ),
      
      
      ########################################################################################################
      ## Second Tab on Dashboard
      ##
      ## This tab contains the interface to new 'unseen' credit card transaction records which can then be
      ## submitted (vai API) to the Azure hosted predictive fraud model I created in Azure ML Studio (classic)
      ##
      ########################################################################################################
      tabItem(
        tabName = "fraud_interface",
        
        fluidRow(
          h1("Credit Card Fraud : Real Time Scoring Interface")
        ),
        
        br(),
        fluidRow(
          h3("Credit Card Fraud : Attributes of a given transaction are read and displayed. Submit data for Fraud Prediction Outcome")
        ),
        
        br(),
        br(),
        fluidRow(
          h4("Select a new credit card transaction file..")
        ),
        fluidRow(csvFileUI("NewCC_Trxn_datafile")),  ## Set up UI elements for file selection dialog
        fluidRow(
          column(16,
                 DT::dataTableOutput("credit_card_file_txns")
          )
        ),
        
        br(),
        fluidRow(
          column(12,DT::dataTableOutput("selected_cc_trxn"))
        ),
        
        br(),
        br(),
        br(),
        br(),
        fluidRow(
          actionButton("apiTxnRow"," Score Selected CC Trxn - You MUST Select a entry first.. "),
          p(" Click this button to display if chosen TRXN scores as fraudulent")
        ),
        
        fluidRow(
          verbatimTextOutput("cctxn_id"),
          tags$head(tags$style("#cctxn_id{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
          )
          )
        ),
        br(),
        br(),
        br(),
        br(),
        fluidRow(
          actionButton("apiBtn","Call Fraud Predictive Model"),
          p(" Click this button to invoke the Credit Card Fraud model and return full attributes/score/label")
        ),
        
        fluidRow(
          verbatimTextOutput("model_result")
        )
      )
      
    )
  )
  
)


# Shiny Server Function

server <- function(input, output, session) {
  
  ## Call function to activate file selection dialog box on 2nd Tab
  ## This allows the user to select a given file with a set of 'new' transactions
  fCCTrxn_datafromfile <- csvFileServer("NewCC_Trxn_datafile", stringsAsFactors = FALSE)
 
  
  ## Set up dataselect routines for user interactions on 2nd tab
  v <- reactiveValues()
  v$s <- NULL
  
  
  
  ##########################################################################################
  ## ---    1st Tab Functions
  ##########################################################################################
  
  # Generate Pie Chart to show proportion of Fraud and non-Fraud transactions in the dataset
  output$plot1 <- renderPlotly({
    
    
    frdPlot <- generateFraudBlananceChart(ds_cctxns)
    frdPlot

    
  })
  

  
  # Generate Credit Card Trxn Amount BoxPlot against Fraud Outcome and remove outliers  ##Fraud' is output variable
  output$plot2 <- renderPlot({
    
    fBxPlt1 <- frdBoxPlot_NoOutliers(ds_cctxns)
    fBxPlt1


  })
  
  
  # Generate Credit Card Trxn Amount BoxPlot against Fraud Outcome and include outliers  ##Fraud' is output variable
  output$plot3 <- renderPlot({
    
    fBxPlt2 <- frdBoxPlot_WithOutliers(ds_cctxns)
    fBxPlt2

    
  })
  
  
  
  # Generate Comparison Matrix for Fraud occurrences vs number of times a PIN was used in the transaction
  output$plot4 <- renderPlot({
    
    ## Call function and return plot to UI section
    pinPlot <- fGetPlot_PinInd(ds_cctxns)
    
    
  })
  
  # Generate Comparison Matrix for Fraud occurrences vs number of times the transaction was flagged as an 
  # ECommerce transaction
  output$plot5 <- renderPlot({
    
    ## Call function and return plot to UI section
    eCommPlot <- fGetPlot_EComm(ds_cctxns)
    
    
  })
  

  ## The table will be sorted on Fraud/non-Fraud that shows a breakdown of fraud based
  ## on whether the customer was physically present at the credit card transaction 
  output$custPresent <- renderDataTable({
    
    # Call function the get datatable and return plot to UI section
    # after formating the datatable output
    datatable(fGetPlot_CustPres(ds_cctxns),
                options = list(dom = 't',
                               order = list(c(0 , 'asc'))),
                rownames = FALSE,
                colnames = c('Fraud?','Is Customer Present','No of Trxnz'),
                filter = "none")

    
  })
  
  # Generate 100% Stacked Bar Chart with Proportion of Fraud Per Region
  output$plot6 <- renderPlot({
    
    ## Call function and return plot to UI section
    frdGeoBars <- fGetPlot_frdGeo(frdCntReport)
    frdGeoBars
    
  })
  


  
  ##########################################################################################
  ## ---    2nd Tab Functions
  ##########################################################################################
  
  
  data <- reactive({
    
    cc_Txns
    
  })
  
  
  output$credit_card_file_txns <- DT::renderDataTable({
    datatable(fCCTrxn_datafromfile(),
              selection = "single", 
              extensions = 'FixedColumns',
              options = list(
                autoWidth = TRUE,
                dom = 't',
                scrollX = TRUE,
                fixedColumns = TRUE
              )
    )
  })


  observe({
    if(!is.null(input$credit_card_file_txns_rows_selected)){
      
      v$s <- input$credit_card_file_txns_rows_selected
      
    }
  })
  

  output$selected_cc_trxn <- DT::renderDataTable({
    datatable({
                dataTableProxy(outputId = 'selected_cc_trxn') %>%
                  hideCols(hide = columns2hide)
                  fCCTrxn_datafromfile()[v$s,]
              },
              options = list(dom = 't')
             )
  })
  
  
  
  ## Function to parse CC Trxn Row into parameters for API call
  get.arg.list.from.cctrxn <- function(){
    
    # Prepare parameter list of given credit card transaction
    chosen_cc_trxns <- fCCTrxn_datafromfile()[v$s,]
    len_list <- length(chosen_cc_trxns)
    arg.list <- list()
    
    # Start reading cc trxn record on first entry in row
    i <- 1
    while(i<(len_list+1)){  # This Reflects the number of attribute entries in the cc_trxn record  
      
      arg.list <- append(arg.list, chosen_cc_trxns[1,i])
      i <- i + 1
      
    }
    
    return(arg.list)
    
  }
  
  
  
  ## Display Basic Score Prediction Result
  idText <- eventReactive(input$apiTxnRow, {


    # Call function to prepare list of attributes for the API call to the predictive Fraud model
    api.arg.list.score <- get.arg.list.from.cctrxn()
        
    # Indicate that only the model score is required
    api.arg.list.score <- append(api.arg.list.score, "Score_Only")
    
    # Pass Parameters for API Call - Full Production Model
    do.call(api_call$AZURE_9C_CCFraud_APICall$Print9CFraudModelResult, api.arg.list.score)
        
    
  })
  
  
  
  # Display CC TXN Score
  output$cctxn_id <- renderPrint({
    idText()
  })
  
  
  observeEvent(input$clearEntries, {
    n <- isolate(input$clearEntries)
    if (n == 0) return()
    updateTextInput(session, "cctxn_id", value = "test")
    
  })

  
  
  ## Display Full Output of Score Predictive Model
  ntext <- eventReactive(input$apiBtn, {
    
    # Call function to prepare list of attributes for the API call to the predictive Fraud model
    api.arg.list <- get.arg.list.from.cctrxn()
    
    # Indicate that the full model output is required
    api.arg.list <- append(api.arg.list, "Full_Output")

    # Pass Parameters for API Call - Full Production Model
    do.call(api_call$AZURE_9C_CCFraud_APICall$Print9CFraudModelResult, api.arg.list)

  })
  

  
  # Call Fraud Model API and display result
  output$model_result <- renderPrint({
    ntext()
  })

  
  
  ##############################################################################################
  # -------  Text Ouput - Used for on screen explanation of the visualization graphs  ---------
  ##############################################################################################
  

  # text output for dataset structure  description
  sHTML_for_Dataset_structure_desc=
    '<p style="color:black; font-size: 12pt">
        Credit Card Fraud Dataset : Structure : This Dashboard TAB displays a number of key EDA visualizations 
        for the credit card transaction data used in this project</p>
      <p> </p>      
      <p> </p>'
   output$text.1 <- renderUI({
    tags$div(
      HTML(sHTML_for_Dataset_structure_desc)
    )
  })
  


  # text output for Pie-chart
  sHTML_for_PieChart_desc_header=
    '<p style="color:black; font-size: 12pt">
        <p>&emsp;&emsp;&emsp;&emsp;
        Pie Chart : Ratio of Positive Fraud Outcomes (1) to Non-Fraud (0) in overall dataset</p>
        <p></p>'
  output$text.2 <- renderUI({
    tags$div(
      HTML(sHTML_for_PieChart_desc_header)
    )
  })
  


  # text output for Fraud Trxn Amount BotPlot against Fraud outcome - Outliers Removed
  sHTML_for_FrdAmtBoxPlots_desc_BoxPlot_NoOut=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        Analysis of range of transaction amounts against Fraud outcomes....Outliers Removed 
        <p></p>
        <p></p><p></p><p></p><p></p>
        <p></p>'
  output$text.3 <- renderUI({
    tags$div(
      HTML(sHTML_for_FrdAmtBoxPlots_desc_BoxPlot_NoOut)
    )
  })
  
  
  # Text output for Transaction Amount BoxPlot against Fraud outcome - Outliers Included
  sHTML_for_FrdAmtBoxPlots_desc_BoxPlot_Out=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        Analysis of range of transaction amounts against Fraud outcomes....Outliers included to show potential to skew modelling process
        <p></p>
        <p></p><p></p><p></p><p></p>
        <p></p>'
  output$text.4 <- renderUI({
    tags$div(
      HTML(sHTML_for_FrdAmtBoxPlots_desc_BoxPlot_Out)
    )
  })
  
  
  # text output for Four Plots on PIN and ECommerce Flags
  sHTML_for_FrdPinEComm_desc_BoxPlot=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        Credit card fraud has a higher incidence when a PIN is not used. ECommerce transactions are also a key
        warning flag.
        <p></p>
        <p></p><p></p><p></p><p></p>
        <p></p>'
  output$text.6 <- renderUI({
    tags$div(
      HTML(sHTML_for_FrdPinEComm_desc_BoxPlot)
    )
  }) 
  
  # text output for Table of Fraud vs Customer Present at Transactions
  sHTML_for_FrdCustPres_desc_BoxPlot=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        The physical present of a customer at the point of transaction had an influence on the potential
        for credit card fraud
        <p></p>
        <p></p><p></p><p></p><p></p>
        <p></p>'
  output$text.7 <- renderUI({
    tags$div(
      HTML(sHTML_for_FrdCustPres_desc_BoxPlot)
    )
  }) 
  
  # text output for Fraud % BotPlot against Geographical Location
  sHTML_for_FrdGeoVars_desc_BoxPlot=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        Analysis of pecentage of Fraud transaction by geographical regions. The vast majority of transactions are from the 
        NA region but the patterns outside of the Americas are interesting.
        <p></p>
        <p></p><p></p><p></p><p></p>
        <p></p>'
  output$text.8 <- renderUI({
    tags$div(
      HTML(sHTML_for_FrdGeoVars_desc_BoxPlot)
    )
  })
  

  
}

shinyApp(ui, server)