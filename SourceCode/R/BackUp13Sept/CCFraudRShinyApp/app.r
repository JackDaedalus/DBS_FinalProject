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
#########################


####################################################################################################
#devtools::install_github("RevolutionAnalytics/AzureML")
library("AzureML")

# R Code for project split into modular files. Set up calls to functions in R files in /Azure sub folder
api_call <- modules::use("Azure")
ui_data <- modules::use("UserApp")
source("UserApp/UI_FileSelection.R")




####################################################################################################


####################################################################################################
# Read Azure Hosted Credit Card Fraud Dataset for Prototype
ds_cctxns = api_call$AZURE_CreditCardFraud_Download$download2KPrototypeCCTxns()
ds_cctxns$Fraud <- as.factor(ds_cctxns$Fraud)
levels(ds_cctxns$Fraud) <- c("Not Fraud", "Fraud")


## Temporary local dataset for testing main Production Fraud Predictive Model
cc_Txns = ui_data$DATA_FileSelection$ReturnDSWithSelectedFeatures()

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
      
      tabItem(
        tabName = "dataviz",
        fluidRow(h1("PROJECT : Higher Diploma in Science in Data Analytics : Module B8IT110 : March 2019 Intake (Fri/Sat)")),
        fluidRow(h1("Ciaran Finnegan : Student Number : 10524150")),
        ## Box Plot Analysis of Transaction Amounts vs Fraud Results
        fluidRow(
          # Heading for dataset ----
          htmlOutput("text.1")
        ),
        fluidRow(
          # Heading for pie chart ----
          htmlOutput("text.5")
        ),
        fluidRow(
          # Output : Pie chart of Fraud Outcomes
          box( 
            width = 7,
            title = "Pie Chart : Fraud Outcomes",
            color = "blue",
            ribbon = TRUE,
            title_side = "top right",
            column(
              width = 7,
              plotlyOutput("plot4", height = 250)
            )
          )
        ),
        br(),
        br(),
        br(),
        fluidRow(
          htmlOutput("text.10"),
          box( 
            width = 16,
            title = "BoxPlots : Transaction Amount Values Against Fraud Result : NO Outliers", 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot9", height = 350)
            )
          )
        ),
        br(),
        br(),
        br(),
        fluidRow(
          htmlOutput("text.11"),
          box( 
            width = 16,
            title = "BoxPlots : Transaction Amount Values Against Fraud Result : WITH Outliers", 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot10", height = 350)
            )
          )
        ),
        # fluidRow(
        #   htmlOutput("text.4"),
        #   box( 
        #       width = 16,
        #       title = "Scatterplot : Credit Card Fraud Dataset",
        #       color = "blue",
        #       ribbon = TRUE,
        #       title_side = "top right",
        #       column(
        #         14,
        #         plotOutput("plot3", height = 350)
        #       )
        #     )
        # ),
        br(),
        br(),
        br(),
        fluidRow(
            htmlOutput("text.7"),
            box( 
              width = 16,
              title = "Histogram : Credit Card Transaction Amounts", # 
              color = "red",
              ribbon = TRUE,
              title_side = "top right",
              column(
                14,
                plotOutput("plot6", height = 300)
              )
            )
        ),
        br(),
        br(),
        br(),
        # Output: Verbatim text for dataset summary ----
        verbatimTextOutput("summary")
        
      ),
      
      
      
      # Second Tab
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
        fluidRow(csvFileUI("NewCC_Trxn_datafile")),
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
  
  fCCTrxn_datafile <- csvFileServer("NewCC_Trxn_datafile", stringsAsFactors = FALSE)
  
  
  v <- reactiveValues()
  v$s <- NULL
  

  ## 1st Tab Functions

  # Generate a summary of the dataset structure----
  output$summary <- renderPrint({

    # dataset <- str(ds_cctxns)
    # summary(dataset)
    tst <- TestFunctionReturn()
    tst
    
    })
  

  
  # Generate Pie Chart
  output$plot4 <- renderPlotly({
    
    # Generate a count by Fraud Outcome in dataset   ##'Fraud' is output variable
    values <- ds_cctxns %>%
      group_by(Fraud) %>%
      summarize(count = n())

    # Set Colours 
    colours <- c('rgb(27, 158, 119)', 'rgb(217, 95, 2)')#"#1B9E77" "#D95F02"
    
    labels = c('Non-Fraud','Fraud')
    
    plot_ly(data = values,type='pie', 
            labels=labels, 
            values=~count,
            text = ~count,
            marker = list(colors = colours,
                          line = list(color = '#FFFFFF', width = 1)))
    
    
  })
  

 
  
  # Generate Credit Card Trxn Amount BoxPlot against Fraud Outcome and remove outliers  ##Fraud' is output variable
  output$plot9 <- renderPlot({
    
    p0 = ggplot(ds_cctxns, aes(x = Fraud, y = AmountOrig, col = Fraud, fill = Fraud)) +
      geom_boxplot(outlier.shape = NA) +
      ylab("Credit Card Trxn Amount") +
      scale_color_manual(values = c("red", "black")) +
      scale_fill_manual(values = c("red", "black"))
    
    # Rescale the Box Plot to Remove the outliers - only focus on quartiles and whiskers
    sts <- boxplot.stats(ds_cctxns$AmountOrig)$stats
    p1 = p0 + coord_cartesian(ylim = c(sts[2]/2,max(sts)*1.35))
    p1

  })
  
  
  # Generate Credit Card Trxn Amount BoxPlot against Fraud Outcome and include outliers  ##Fraud' is output variable
  output$plot10 <- renderPlot({
    
    ggplot(ds_cctxns, aes(x = Fraud, y = AmountOrig, col = Fraud, fill = Fraud)) +
      geom_boxplot(alpha = 0.2,
                   outlier.shape=8,
                   outlier.size=3) +
      ylab("Credit Card Trxn Amount") +
      scale_color_manual(values = c("red", "black")) +
      scale_fill_manual(values = c("red", "black"))

    
  })
  
  
  
  
  # Generate Scatterplot
  output$plot3 <- renderPlot({
    
    ds_cctxns$AmountOrig <- ifelse(ds_cctxns$AmountOrig < 5000, "<5000 $", ">= 5000 $")
    
    ggplot(aes(x = CardType, y = MerchantCategory), data = ds_cctxns) +
      geom_point(aes(col = factor(Fraud), shape = factor(AmountOrig)), size = 3) +
      scale_color_brewer(name = "Fraus", palette = "Dark2") +
      scale_shape(name = "Trxn Amt") 
    
  })
  
  
  # Generate Credit Card Transaction Amount Histogram
  output$plot6 <- renderPlot({
    
    ggplot(aes(x = AmountOrig), data=ds_cctxns) +
      geom_histogram(binwidth=100, color='black', fill = "green") +
      scale_x_continuous(limits=c(0,20000), breaks=seq(0,20000,1000)) +
      xlab("CC Trxn Amt") +
      ylab("Frequency By Trxn Amount")
    
    
  })
  
  
  
  
  ## 2nd Tab functions
  
  
  data <- reactive({
    cc_Txns
  })
  
  
  output$credit_card_file_txns <- DT::renderDataTable({
    datatable(fCCTrxn_datafile(),
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
                data()[v$s,]
              },
              options = list(dom = 't')
             )
  })
  
  
  
  ## Function to parse CC Trxn Row into parameters for API call
  get.arg.list.from.cctrxn <- function(){
    
    # Prepare parameter list of given credit card transaction
    chosen_cc_trxns <- data()[v$s,]
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

  
  
  ##########################################################################
  # -------  Text Ouput ----------------------------------------------------
  ##########################################################################
  

  # text output for dataset structure  description
  sHTML_for_Dataset_structure_desc=
    '<p style="color:black; font-size: 12pt">
        Credit Card Fraud Dataset : Structure </p>
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
        Pie Chart : Ratio of Positive Fraud Outcomes (1) to Non-Fraud (0)</p>
        <p></p>'
  output$text.5 <- renderUI({
    tags$div(
      HTML(sHTML_for_PieChart_desc_header)
    )
  })
  

  
  # text output for BMI Histogram
  sHTML_for_BMIBoxPlots_desc_Hist=
    '<p style="color:black; font-size: 12pt">
        <p></p>
        <p></p>
        Credit Card Transaction - This is a new data element 
        <p></p>
        <p></p><p>Click the Submit Button to invoke real time call to Fraud Predictive Model..</p>
        <p></p>
        <p></p>'
  output$text.8 <- renderUI({
    tags$div(
      HTML(sHTML_for_BMIBoxPlots_desc_Hist)
    )
  })
  
  
  
  # text output for BMI BotPlot against outcome
  sHTML_for_BMIBoxPlots_desc_BoxPlot_NoOut=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        Analysis of range of transaction amounts against Fraud outcomes....Outliers Removed
        <p></p>
        <p></p><p></p><p></p><p></p>
        <p></p>'
  output$text.10 <- renderUI({
    tags$div(
      HTML(sHTML_for_BMIBoxPlots_desc_BoxPlot_NoOut)
    )
  })
  
  
  # text output for BMI BotPlot against outcome
  sHTML_for_BMIBoxPlots_desc_BoxPlot=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        Analysis of range of transaction amounts against Fraud outcomes....Outliers Included
        <p></p>
        <p></p><p></p><p></p><p></p>
        <p></p>'
  output$text.11 <- renderUI({
    tags$div(
      HTML(sHTML_for_BMIBoxPlots_desc_BoxPlot)
    )
  })
  

  
}

shinyApp(ui, server)