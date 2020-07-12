## Higher Diploma in Science in Data Analytics : Project : Module Code B8IT110
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## July 2020

## Application Part One - R Shniy Application performing descriptive analytics on Credit Card Fraud dataset
## The visualisations are presented through a Shiny UI dashboard



## This Shiny R application answer uses many standard R libraries, including an open source R Shiny package for 
## a Semantic Dashbaord. Details of this open source package can be found here :...
## https://appsilon.com/semantic-dashboard-new-open-source-r-shiny-package/

#########################
library(shiny)
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
#########################



####################################################################################################
# Read the Pima Indian Diabetes CSV File and perform some initial data clean up and manipulation
#diabetes <- read.csv("pima-indians-diabetes.csv")
#diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA # replaces all zero values from column two to six with NA
#diabetes <- na.omit(diabetes) # now we omit all NA values
#diabetes$Outcome <- as.factor(diabetes$Outcome)
#levels(diabetes$Outcome) <- c("No Diabetes", "Diabetes")
####################################################################################################


diabetes <- read.csv("german.data.with.headings.csv")
diabetes$credit_risk <- as.factor(diabetes$credit_risk)
levels(diabetes$credit_risk) <- c("No Fraud", "Fraud")


####################################################################################################

# Below are a set of R functions to perform relatively straightforward actions...





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
          htmlOutput("text.10"),
          box( 
            width = 16,
            title = "BoxPlots : Transaction Amount Values Against Fraud Result", 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot9", height = 350)
            )
          )
        ),
        fluidRow(
          # Heading for dataset ----
          htmlOutput("text.1"),
          # Heading for pie chart ----
          htmlOutput("text.5")
          ),
        fluidRow(
          # Output: Verbatim text for dataset summary ----
          verbatimTextOutput("summary"),
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
        )
      ),
      tabItem(
        tabName = "fraud_interface",
        fluidRow(
          h1("Credit Card Fraud : Real Time Scoring Interface")
        ),
        fluidRow(
          h3("Credit Card Fraud : Attributes of a given transaction are read and displayed. Submit data for Fraud Prediction Outcome")
        ),
        fluidRow(
          actionButton("apiBtn","Call Fraud Predictive Model"),
          p("Click this button to invoke the Credit Card Fraud model")
        ),
        fluidRow(
          verbatimTextOutput("model_result")
        )
      )
    )
  )
  
)


# Shiny Server Function

server <- function(input, output) {


  # Generate a summary of the dataset structure----
  output$summary <- renderPrint({
    dataset <- str(diabetes)
    summary(dataset)
  })
  

  
  # Generate Pie Chart
  output$plot4 <- renderPlotly({
    
    # Generate a count by Diabetes Outcome in dataset   ##credit_risk is output variable
    values <- diabetes %>%
      group_by(credit_risk) %>%
      summarize(count = n())

    # Set Colours to match the Scatterplot on same tab on dashbaord
    colours <- c('rgb(27, 158, 119)', 'rgb(217, 95, 2)')#"#1B9E77" "#D95F02"
    
    labels = c('Non-Fraud','Fraud')
    
    plot_ly(data = values,type='pie', 
            labels=labels, 
            values=~count,
            text = ~count,
            marker = list(colors = colours,
                          line = list(color = '#FFFFFF', width = 1)))
    
    
  })
  

 
  
  # Generate BMI BoxPlot against Outcome and show outliers  ##credit_risk is output variable
  output$plot9 <- renderPlot({
    
    ggplot(diabetes, aes(x = credit_risk, y = diabetes$credit_amount, col = credit_risk, fill = credit_risk)) +
      geom_boxplot(alpha = 0.2, 
                   outlier.shape=8,
                   outlier.size=3) +  
      ylab("Credit Amount") +
      scale_color_manual(values = c("red", "black")) +
      scale_fill_manual(values = c("red", "black"))
    
  })
  
  
  ntext <- eventReactive(input$apiBtn, {
    
    api_call <- modules::use("Azure")
    ##api_call$invoke$fraud_model()
    api_call$AZURE_GermanCredit_APICall$printFraudModelResult()

    
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
        <p>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;
        &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;
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
  sHTML_for_BMIBoxPlots_desc_BoxPlot=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        Analysis of range of transaction amounts against Fraud outcomes....
        <p></p>
        <p></p><p></p><p></p><p></p>
        <p></p>'
  output$text.10 <- renderUI({
    tags$div(
      HTML(sHTML_for_BMIBoxPlots_desc_BoxPlot)
    )
  })
  

  
}

shinyApp(ui, server)