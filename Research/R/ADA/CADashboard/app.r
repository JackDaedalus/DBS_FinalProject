## CA One Advanced Data Analytics : Module Code B8IT109
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## May 2020

## Question One - R Shniy Application performing descriptive analytics on Pima Indians dataset
## The answers are presented through a Shiny UI dashboard
## Each sub section of the Question is answered on a seperate tab within the dashbaord
## The initial tab displayed is the answer to Question 1(a)


## This CA answer uses many standard R libraries, including an open source R Shiny package for 
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
#########################



####################################################################################################
# Read the Pima Indian Diabetes CSV File and perform some initial data clean up and manipulation
diabetes <- read.csv("pima-indians-diabetes.csv")
diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA # replaces all zero values from column two to six with NA
diabetes <- na.omit(diabetes) # now we omit all NA values
diabetes$Outcome <- as.factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("No Diabetes", "Diabetes")
####################################################################################################


####################################################################################################

# Below are a set of R functions to perform relatively straightforward actions. They are used to 
# simplify the operations within the Server function for this Shiny R application as descriptive
# information is being passed based to the UI function for display.


# Calculate mean of attribute list - pass an attribute list from Pima dataset
dataset.attr.mean <- function(y) {
  
  return(mean(y))
  
}

# Calculate median of attribute list - pass an attribute list from Pima dataset
dataset.attr.median <- function(y) {
  
  return(median(y))
  
}

# Calculate median of attribute list - pass an attribute list from Pima dataset
# Start by creating a decicated 'Mode' function - as shown in lectures
dataset.attr.mode <- function(v) {
  
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
  
}


# Calculate range of values of an attribute list - pass an attribute list from Pima dataset
dataset.attr.range <- function(v) {
  
  # Range of values in <> attribute
  rDPF = range(v)
  RangeDPF = rDPF[2] - rDPF[1]
  RangeDPF
  
}

# Calculate variance of values of an attribute list - pass an attribute list from Pima dataset
dataset.attr.var <- function(v) {
  
  V=var(v)
  
}

# Calculate standard deviation of values of an attribute list - pass an attribute list from Pima dataset
dataset.attr.stddev <- function(v) {
  
  s=sd(v)
  
}


# Calaculate a Sigma One Interval as part of the output for the Chebyshev's rule questions in 1(c) of CA One.
# This function takes in the given attribute list from the Pima Indian dataset as a parameter 'v'
dataset.attr.OneSigmaInterval <- function(v){
  
  # Calculate mean (xbar) of attribute values in Pima Indian dataset
  xbar=mean(v)
  
  # Standard Deviation of attribute values in Pima Indian dataset
  s=sd(v)
  
  
  # Compute one-sigma interval estimation and find any outliers
  Lwr=xbar - s 
  Upr=xbar + s 
  Interval = c(Lwr, Upr)
  
  dispOneSigmaInterval = Interval  # Populate variable for screen dsiplay in following function
  
  sort(dispOneSigmaInterval) # Sorting the interval values improves the presentation of the data
  
  
}


# This function identifies outliers in attribute values, if any, beyond the given range
# v is the attribute from dataset
# Lwr and Upr are the interval boundaries as calculated in a previous function
dataset.attr.findOutliers <- function(v, Lwr, Upr){
  
  
  # Create and initial empty vector set for Outlier values
  AttrOutliers = c()
  
  # Loop through the values in the attribute in the Pima Indian dataset 
  # select those values that do not value within the one-sigma interval
  
  for (i in 1:length(v)){  # Iterate through list of attribute values in Pima Indian dataset
    
    if(v[i]<Lwr|v[i]>Upr){   # If given attribute value is outside Interval range..
      
      attrOut = v[i]  # ...select it as an outlier
      
    }
    
    else {
      
      attrOut=NULL # If the value is within the interval range, then do nothing 
      
    }
    
    # Build up list of outliers with values found outside interval range
    AttrOutliers = c(AttrOutliers, attrOut)  
    
  } 
  
  
  sort(AttrOutliers)
  
  
}


####################################################################################################

# Shiny UI Function

ui <- dashboardPage(
  
  
  dashboardHeader(color = "blue", title = "ADA CA One", inverted = TRUE),
  
  dashboardSidebar(
    size = "wide", color = "teal",
    sidebarMenu(
      menuItem(tabName = "dataviz", text = "CA One : Q1(a) : Dataset Overview", icon = icon("tv")),
      menuItem(tabName = "ca_one_b", text = "CA One : Q1(b)", icon = icon("save")),
      menuItem(tabName = "ca_one_c", text = "CA One : Q1(c)", icon = icon("save")),
      menuItem(tabName = "ca_one_d", text = "CA One : Q1(d)", icon = icon("save"))
  )),
  
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "dataviz",
        fluidRow(h1("CA ONE : Advanced Data Analytics : Module B8IT109")),
        fluidRow(h1("Ciaran Finnegan : Student Number : 10524150")),
        fluidRow(h2("Question 1 : Descriptive Analytics on rea world dataset : Pima Indian Diabetes Database")),
        fluidRow(h3("This is a well known dataset in Machine Learning. The datasets consist of several medical predictor (independent) variables and one target (dependent) variable, Outcome - did the subject develop diabates. Independent variables include the number of pregnancies the patient has had, their BMI, insulin level, age, and so on.")),
        fluidRow(h3("The dataset contains a mixture of continous and discrete variables, upon which I chose to perform various descriptive analysis.")),
        fluidRow(h3("This tab represents the answer to Question 1(a) of CA One and is a general set of descriptive overviews of the data.")),
        fluidRow(h3("The other tabs cover the analysis of specific data attributes based on the CA One questions in 1(b) - 1(d).")),
        fluidRow(h4("The analysis is best viewed in a browser or maximised.")),
        fluidRow(
          htmlOutput("text"),
          dataTableOutput("pimatable")
        ),
        fluidRow(
          # Output: Verbatim text for data summary ----
          htmlOutput("text.1"),
          verbatimTextOutput("summary")
        ),
        fluidRow(
          htmlOutput("text.2"),
          box( 
            width = 16,
            title = "Age Histogram : Pima Indians Diabetes Dataset", # Formerly 'AGE
            color = "blue",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot1", height = 350)
              )
            )
        ),
        fluidRow(
          htmlOutput("text.3"),
          box( 
            width = 16,
            title = "All Histograms : Pima Indians Diabetes Dataset",
            color = "blue",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot2", height = 650)
            )
          )
        ),
        fluidRow(
          htmlOutput("text.4"),
          box( 
            width = 16,
            title = "Scatterplot : Pima Indians Diabetes Dataset",
            color = "blue",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot3", height = 350)
            )
          )
        ),
        fluidRow(
          htmlOutput("text.5"),
          box( 
            width = 8,
            title = "Pie Chart : Diabetes Outcomes",
            color = "blue",
            ribbon = TRUE,
            title_side = "top right",
            column(
              width = 8,
              plotlyOutput("plot4", height = 250)
            )
          ),
          htmlOutput("text.6"),
          box( 
            width = 8,
            title = "Correlation Matrix",
            color = "green",
            ribbon = TRUE,
            title_side = "top right",
            column(
              width = 8,
              plotOutput("plot5", height = 250)
            )
          )
        )
      ),
      tabItem(
        tabName = "ca_one_b",
        fluidRow(
          h1("CA One - Question 1(b) : Central and Variational Measures for a continous attribute in dataset")
        ),
        fluidRow(
          h3("Diabetes Pedigree Function : Continous attribute chosen from dataset")
        ),
        fluidRow(
            htmlOutput("text.7"),
            box( 
              width = 16,
              title = "Histogram : Diabetes Pedigree Function", # 
              color = "red",
              ribbon = TRUE,
              title_side = "top right",
              column(
                14,
                plotOutput("plot6", height = 300)
              )
            )
        ),
        fluidRow(
          htmlOutput("text.12.1")
        ),
        fluidRow(
          verbatimTextOutput("DBFMean")
        ),
        fluidRow(
          htmlOutput("text.12.2")
        ),
        fluidRow(
          verbatimTextOutput("DBFMedian")
        ),
        fluidRow(
          htmlOutput("text.12.3")
        ),
        fluidRow(
          verbatimTextOutput("DBFMode")
        ),
        fluidRow(
          htmlOutput("text.12.4")
        ),
        fluidRow(
          verbatimTextOutput("DBFRange")
        ),
        fluidRow(
          htmlOutput("text.12.5")
        ),
        fluidRow(
          verbatimTextOutput("DBFVariance")
        ),
        fluidRow(
          htmlOutput("text.12.6")
        ),
        fluidRow(
          verbatimTextOutput("DBFStdDev")
        ),
      ),
      tabItem(
        tabName = "ca_one_c",
        fluidRow(
          h1("CA One - Question 1(c) : Using Chebyshev's Rule")
        ),
        fluidRow(
          h3("Glucose : Attribute chosen from dataset")
        ),
        fluidRow(
          htmlOutput("text.11"),
          box( 
            width = 16,
            title = "Histogram : Glucose Attribute Values", # 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot11.1", height = 200)
            )
          )
        ),
        fluidRow(
          htmlOutput("text.11.2")
        ),
        fluidRow(
          verbatimTextOutput("OneSigmaInterval")
        ),
        fluidRow(
          htmlOutput("text.11.3")
        ),
        fluidRow(
          column(3, offset=0, verbatimTextOutput("gluOutliers"))
        )
      ),
      tabItem(
        tabName = "ca_one_d",
        fluidRow(
          h1("CA One - Question 1(d) : Box Plot Techniques")
        ),
        fluidRow(
          h3("BMI : Attribute chosen from dataset to demonstrate Box Plot Technique and the identification of outliers")
        ),
        fluidRow(
          htmlOutput("text.8"),
          box( 
            width = 16,
            title = "Histogram : BMI Attribute Values", # 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot7", height = 200)
            )
          )
        ),
        fluidRow(
          htmlOutput("text.9.1")
        ),
        fluidRow(
          htmlOutput("text.9.2")
        ),
        fluidRow(
          verbatimTextOutput("bmiOutliers")
        ),
        fluidRow(
          box( 
            width = 16,
            title = "BoxPlots : BMI Attribute Values", # 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot8", height = 350)
            )
          )
        ),
        fluidRow(
          htmlOutput("text.10"),
          box( 
            width = 16,
            title = "BoxPlots : BMI Attribute Values Against Outcome", # 
            color = "red",
            ribbon = TRUE,
            title_side = "top right",
            column(
              14,
              plotOutput("plot9", height = 350)
            )
          )
        )
      )
    )

  )
  
)


# Shiny Server Function

server <- function(input, output) {

  # Generate Table view of top records in Diabetes dataset
  output$pimatable <- DT::renderDataTable({
    DT::datatable(diabetes, options = list(lengthMenu = c(5, 30, 50), pageLength = 3))
  })
  
  # Generate a summary of the dataset structure----
  output$summary <- renderPrint({
    dataset <- str(diabetes)
    summary(dataset)
  })
  
  # Generate AGE Histrogram
  output$plot1 <- renderPlot({
    
    ggplot(aes(x = Age), data=diabetes) +
      geom_histogram(binwidth=1, color='black', fill = "green") +
      scale_x_continuous(limits=c(20,85), breaks=seq(20,85,5)) +
      xlab("Age") +
      ylab("Frequency By Age")

    
  })
  
  # Generate Total Histrogram
  output$plot2 <- renderPlot({
    
    #d <- melt(diabetes[,-c(2:4)])
    d <- melt(diabetes)
    ggplot(d,aes(x = value, fill=variable)) + 
      facet_wrap(~variable,scales = "free_x", nrow=3,ncol=3) + 
      geom_histogram(position="identity")
    
  })
  
  # Generate Scatterplot
  output$plot3 <- renderPlot({
    
    diabetes$Age <- ifelse(diabetes$Age < 30, "<30 yrs", ">= 30 yrs")
    
    ggplot(aes(x = Glucose, y = BMI), data = diabetes) +
      geom_point(aes(col = factor(Outcome), shape = factor(Age)), size = 3) +
      scale_color_brewer(name = "Outcome", palette = "Dark2") +
      scale_shape(name = "Age") 
    
  })
  
  # Generate Pie Chart
  output$plot4 <- renderPlotly({
    
    # Generate a count by Diabetes Outcome in dataset
    values <- diabetes %>%
      group_by(Outcome) %>%
      summarize(count = n())

    # Set Colours to match the Scatterplot on same tab on dashbaord
    colours <- c('rgb(27, 158, 119)', 'rgb(217, 95, 2)')#"#1B9E77" "#D95F02"
    
    labels = c('Healthy','Diabetic')
    
    plot_ly(data = values,type='pie', 
            labels=labels, 
            values=~count,
            text = ~count,
            marker = list(colors = colours,
                          line = list(color = '#FFFFFF', width = 1)))
    
    
  })
  
  # Generate Correlation Plot
  output$plot5 <- renderPlot({
    
    # Generate Correlation Matrix for the Diabetes dataset
    db_cor <- round(cor(diabetes[1:8]),1)
    ggcorrplot(db_cor)
    
    
  })
  
  
  # Generate Diabetes Pedigree Function Histrogram
  output$plot6 <- renderPlot({
    
    ggplot(aes(x = DiabetesPedigreeFunction), data=diabetes) +
      geom_histogram(binwidth=0.1, color='black', fill = "gray") +
      scale_x_continuous(limits=c(0.075,2.5), breaks=seq(0.075,2.5,0.1)) +
      xlab("Diabetes Pedigree Function") +
      ylab("Frequency By Diabetes Pedigree Function")
    
    
  })
  

  
  output$DBFMean <- renderText({
    
    #################################################################################
    # Central Measures - mean, median, amd mode
    #################################################################################
    
    # Return Mean of given attribute value in Pima Indian dataset
    attrMean = dataset.attr.mean(diabetes$DiabetesPedigreeFunction)
    
  })
  
  output$DBFMedian <- renderText({
    
    #################################################################################
    # Central Measures - mean, median, amd mode
    #################################################################################
    
    # Return Median of given attribute value in Pima Indian dataset
    attrMedian = dataset.attr.median(diabetes$DiabetesPedigreeFunction)
    
  })
  
  output$DBFMode <- renderText({
    
    #################################################################################
    # Central Measures - mean, median, amd mode
    #################################################################################
    
    # Return Mode of given attribute value in Pima Indian dataset
    attrMode = dataset.attr.mode(diabetes$DiabetesPedigreeFunction)
    
  })
  
  
  output$DBFRange <- renderText({
    
    #################################################################################
    # Variational Measures : Range/variance/standard deviation
    #################################################################################
    
    # Return Range of given attribute values in Pima Indian dataset
    attrRange = dataset.attr.range(diabetes$DiabetesPedigreeFunction)
    
  })
  
  output$DBFVariance <- renderText({
    
    #################################################################################
    # Variational Measures : Range/variance/standard deviation
    #################################################################################
    
    # Return Range of given attribute values in Pima Indian dataset
    attrVar = dataset.attr.var(diabetes$DiabetesPedigreeFunction)
    
  })
  
  output$DBFStdDev <- renderText({
    
    #################################################################################
    # Variational Measures : Range/variance/standard deviation
    #################################################################################
    
    # Return Range of given attribute values in Pima Indian dataset
    attrSD = dataset.attr.stddev(diabetes$DiabetesPedigreeFunction)
    
  })

  
  
  # Generate BMI Histrogram - Answer to Question 1(d)
  output$plot7 <- renderPlot({
    
    ggplot(aes(x = BMI), data=diabetes) +
      geom_histogram(binwidth=1, color='black', fill = "purple") +
      scale_x_continuous(limits=c(15,70), breaks=seq(15,70,5)) +
      xlab("BMI") +
      ylab("Frequency By BMI")
    
  })
  
  # Generate BMI BoxPlot  and show outliers
  output$plot8 <- renderPlot({
    
    ggplot(diabetes, aes(y = diabetes$BMI)) +
      geom_boxplot(outlier.colour="red", outlier.shape=8,
                   outlier.size=4) +
      ylab("BMI") +
      theme(axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      coord_flip() +
      scale_y_continuous(limits=c(15,70), breaks=seq(15,70,5))

    
  })
  
  
  # Generate BMI BoxPlot against Outcome and show outliers
  output$plot9 <- renderPlot({
    
    ggplot(diabetes, aes(x = Outcome, y = diabetes$BMI, col = Outcome, fill = Outcome)) +
      geom_boxplot(alpha = 0.2, 
                   outlier.shape=8,
                   outlier.size=3) +  
      ylab("BMI") +
      scale_color_manual(values = c("red", "black")) +
      scale_fill_manual(values = c("red", "black"))
    
  })
  
  # Generate Glucose Histrogram - Answer to Question 1(d)
  output$plot11.1 <- renderPlot({
    
    ggplot(aes(x = Glucose), data=diabetes) +
      geom_histogram(binwidth=5, color='black', fill = "orange") +
      scale_x_continuous(limits=c(50,200), breaks=seq(50,200,10)) +
      xlab("Glucose") +
      ylab("Frequency By Glucose")
    
  })
  
  # Generate the list of Glucose outliers after the application of a one-sigma 
  # Chebyshev's rule
  
  output$gluOutliers <- renderPrint({
    
    ## Question One -1(c) 
    # - Take the 'Glucose' attribute, use Chebyshev's rule to propose a one-sigma interval.
    # - Using this interval, specify the outliers
    
    # Select discrete attribute from Pima Indian dataset - Glucose : the Plasma glucose 
    # concentration over 2 hours in an oral glucose tolerance test.
    
    pimaGluAttr = diabetes$Glucose
    
    # Call function to retrun One Sigma Interval
    bounds=dataset.attr.OneSigmaInterval(pimaGluAttr)
    Lwr=bounds[1] # Read Lower bound in interval
    Upr=bounds[2] # Read Upper bound in interval
    
    GlucoseOutliers = dataset.attr.findOutliers(pimaGluAttr,Lwr,Upr)
    GlucoseOutliers

    
  })
  
  output$OneSigmaInterval <- renderPrint({
    
    dataset.attr.OneSigmaInterval(diabetes$Glucose)
    
  })
    
  
  # Generate list of BMI Outliers using Box Plot Technique 
  # Display the list of Outliers to the screen
  output$bmiOutliers <- renderPrint({
    
    
    ## Question One -1(d) 
    # - Take the 'BMI' attribute, use the box Plot technique to identify outliers.
    # - Using this interval, specify the outliers
    
    # Select discrete attribute from Pima Indian dataset - Body mass index (weight in kg/(height in m)^2)
    
    pimaBMIAttr = diabetes$BMI
    
    
    # ---- Calculate required central and variational measures ----
    
    # Calculate mean (xbar) of Gluscose values in Pima Indian dataset
    xbarBMI=mean(pimaBMIAttr)
    
    # Standard Deviation of Gluscose values in Pima Indian dataset
    s=sd(pimaBMIAttr)
    
    
    # ---- Apply Box Plot Technique to detect outliers in BMI values in Pima Indian dataset
    # ---- I did not write a seperate function for this as this code is only called once in this application
    Q = quantile(pimaBMIAttr)
    
    Q1 = Q[2]
    Qu = Q[4]
    
    IQR = Qu - Q1
    
    LW = Q1 - (1.5 * IQR)
    UW = Qu + (1.5 * IQR)
    
    
    bmiOutliers = dataset.attr.findOutliers(pimaBMIAttr,LW,UW)
    bmiOutliers
    
    
  })
  
  
  ##########################################################################
  # -------  Text Ouput ----------------------------------------------------
  ##########################################################################
  
  
  # text output for dataset structure  description
  sHTML_for_Dataset_table=
    '<p style="color:black; font-size: 12pt">
        PIMA Indian Diabetes Dataset : Initial Rows </p>
        <p></p>'
  output$text <- renderUI({
    tags$div(
      HTML(sHTML_for_Dataset_table)
    )
  })
  
  # text output for dataset structure  description
  sHTML_for_Dataset_structure_desc=
    '<p style="color:black; font-size: 12pt">
        PIMA Indian Diabetes Dataset : Structure </p>
      <p> </p>      
      <p> </p>'
   output$text.1 <- renderUI({
    tags$div(
      HTML(sHTML_for_Dataset_structure_desc)
    )
  })
  
  # text output for data description
  sHTML_for_Age_desc_header=
        '<p style="color:black; font-size: 12pt">
        PIMA Indian Diabetes Dataset : Visualisations </p>
        <p></p>
        <p>Profile of Ages in Dataset: The minium age for the sample was set as 21</p>
        <p></p>'
  output$text.2 <- renderUI({
    tags$div(
      HTML(sHTML_for_Age_desc_header)
    )
  })
  
  
  # text output for All Histograms
  sHTML_for_Dataset_desc_header=
    '<p style="color:black; font-size: 12pt">
        <p>Profile of All colums in Dataset</p>
        <p></p>'
  output$text.3 <- renderUI({
    tags$div(
      HTML(sHTML_for_Dataset_desc_header)
    )
  })
  
  # text output for Scatterplot
  sHTML_for_Scatterplot_desc_header=
    '<p style="color:black; font-size: 12pt">
        <p>Scatterplot : Outcome - 0 (Green) denotes non-diabetic, 1 (Amber) denotes diabetic</p>
        
        <p></p>
        This represents relationship of BMI/Glucose measurements to an outcome of diabetes, layering in age into the plot.
        <p></p>
        <p></p>'
  output$text.4 <- renderUI({
    tags$div(
      HTML(sHTML_for_Scatterplot_desc_header)
    )
  })
  
  
  # text output for Pie-chart
  sHTML_for_PieChart_desc_header=
    '<p style="color:black; font-size: 12pt">
        <p>Pie Chart : Ratio of Diabetes Outcomes (1) to Non-Diabetes (0)</p>
        <p></p>'
  output$text.5 <- renderUI({
    tags$div(
      HTML(sHTML_for_PieChart_desc_header)
    )
  })
  
  # text output for Correlation Chart
  sHTML_for_Correlation_desc_header=
    '<p style="color:black; font-size: 12pt">
        <p>Diabetes Datset : Correlation of Attributes</p>
        <p></p>
        Maximise Shiny App Window or view in Browser to view.
        <p></p>'
  output$text.6 <- renderUI({
    tags$div(
      HTML(sHTML_for_Correlation_desc_header)
    )
  })
  
  
  # text output for Diabetes Function Histogram
  sHTML_for_DPFMeasures_desc_header=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p>Histogram : Diabetes Pedigree Function Attribute</p>
        
        <p></p>
        Below are the central and variance measures for this continous attribute within the datast.
        <p></p>
        <p></p>'
  output$text.7 <- renderUI({
    tags$div(
      HTML(sHTML_for_DPFMeasures_desc_header)
    )
  })
  
  
  # text output for BMI Histogram
  sHTML_for_BMIBoxPlots_desc_Hist=
    '<p style="color:black; font-size: 12pt">
        <p></p>
        <p></p>
        Question 1(d) asks - Explain how the box-plot technique can be used to detect outliers. Apply this technique for one attribute of the dataset -
        <p></p>
        <p></p><p>Histogram : BMI - our chosen attribute within the Pima Indian diabetes dataset to investigate for outliers</p>
        <p></p>
        <p></p>'
  output$text.8 <- renderUI({
    tags$div(
      HTML(sHTML_for_BMIBoxPlots_desc_Hist)
    )
  })
  
  
  # text output for BMI BotPlot against outcome
  sHTML_for_How_BoxPlots_Work=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        In this anwser we use BoxPlot technique on full range of BMI values (after data clean up routines) to identifer outliers.
        <p></p>
        A box and whisker plot, also called a box plot, displays the five-number summary of a set of data. 
        The five-number summary is the minimum, first quartile, median, third quartile, and maximum. 
        In a box plot, we draw a box from the first quartile to the third quartile. 
        A vertical line goes through the box at the median.
        <p></p>
        The whiskers go from each quartile to the minimum or maximum.
        <p></p>
        <p></p>
        <p></p>
        This five-number summary divides the data into sections that each contain approximately 25 percent of the data in that set.
        <p></p>See the diagram below (ref: Michael Galarnyk - Towards Data Science)
        <p></p>
        <img src="https://miro.medium.com/max/1400/1*2c21SkzJMf3frPXPAR_gZA.png" alt="Box Plots" width="450"
         height="350">
        <p></p>Median (Q2/50th Percentile): the middle value of the dataset.
        <p></p>First quartile (Q1/25th Percentile): the middle number between the smallest number (not the “minimum”) and the median of the dataset.
        <p></p>Third quartile (Q3/75th Percentile): the middle value between the median and the highest value (not the “maximum”) of the dataset.
        <p></p>Interquartile range (IQR): 25th to the 75th percentile.
        <p></p>
        <p></p>
        <p></p>
        <p></p>
        With these boxplots, we can check the variances assumption within each group for each variable. The 
        bigger the boxplot is, the larger the variance. The horizontal line inside each boxplot 
        represents the median and the lower end and the upper end are the first and third
        quantile respectively. 
        <p></p>The points that are visible beyond the whiskers are considered outliers. 
        <p></p>
        <p></p>
        <p></p>'
  output$text.9.1 <- renderUI({
    tags$div(
      HTML(sHTML_for_How_BoxPlots_Work)
    )
  })
  
  # text output for dataset structure  description
  sHTML_for_BoxPlot_Calculation=
    '<p style="color:black; font-size: 12pt">
        PIMA Indian Diabetes Dataset : Box Plot Technique Calculations to determine outliers in BMI values </p>
        # ---- Calculate required central and variational measures ----
        <p> </p>
        # Calculate mean (xbar) of BMI values in Pima Indian dataset
        <p> </p>
        xbarBMI=mean(pimaBMIAttr)
        <p> </p>
        <p> </p>
        <p> </p>
        # Standard Deviation of BMI values in Pima Indian dataset
        <p> </p>
        s=sd(pimaBMIAttr)
        <p> </p>
        <p> </p>
        <p> </p>
        # ---- Apply Box Plot Technique to detect outliers in BMI values in Pima Indian dataset
        Q = quantile(pimaBMIAttr)
        <p> </p>
        <p> </p>
        Q1 = Q[2]
        <p> </p>
        Qu = Q[4]
        <p> </p>
        <p> </p>
        IQR = Qu - Q1
        <p> </p>
        <p> </p>
        LW = Q1 - (1.5 * IQR)
        <p> </p>
        UW = Qu + (1.5 * IQR)
        <p> </p>      
        <p> </p>
        <p> </p>
        By comparing each value in the BMI dataset we can extract the following list of outliers 
        (which are represented as star shapes in the graph below)..
        <p> </p>'
  output$text.9.2 <- renderUI({
    tags$div(
      HTML(sHTML_for_BoxPlot_Calculation)
    )
  })
  
  
  # text output for BMI BotPlot against outcome
  sHTML_for_BMIBoxPlots_desc_BoxPlot=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        Applying BoxPlot technique BMI in relation to "Outcome" - with Outliers.
        <p></p>
        The visible dots are the outliers in relation to the set of Outcomes in the Pima Indian Diabetes dataset.
        <p></p>
        <p></p>'
  output$text.10 <- renderUI({
    tags$div(
      HTML(sHTML_for_BMIBoxPlots_desc_BoxPlot)
    )
  })
  
  
  # text output for Glucose Histogram and Chebyshev's rule
  sHTML_for_GlucoseCheb_Desc=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        Applying Chebyshevs rule to specify a one-sigma interval and idenitify outliers in the Glucose attribute values
        in the Pima Indian dataset
        <p></p>
        <p></p>
        <p></p>
        <p></p>'
  output$text.11 <- renderUI({
    tags$div(
      HTML(sHTML_for_GlucoseCheb_Desc)
    )
  })
  
  # text output for Glucose Histogram and Chebyshev's rule
  sHTML_for_GlucoseCheb_Calc=
    '<p style="color:black; font-size: 12pt">
        <p></p><p></p><p></p><p></p>
        # - Take the Glucose attribute, then use Chebyshevs rule to propose a one-sigma interval.
        <p></p># - Using this interval, specify the outliers
        <p></p># Select discrete attribute from Pima Indian dataset - Glucose : the Plasma glucose 
        concentration over 2 hours in an oral glucose tolerance test.
        <p></p>
        <p></p>
        # ---- Calculate required central and variational measures ----
        <p></p>
        # Calculate mean (xbar) of Gluscose values in Pima Indian dataset
        <p></p>xbarGlu=mean(pimaGluAttr)
        <p></p>
        # Standard Deviation of Gluscose values in Pima Indian dataset
        <p></p>s=sd(pimaGluAttr)
        <p></p>
        <p></p>
        <p></p># Compute one-sigma interval estimation and find any outliers
        <p></p>Lwr=xbarGlu - s; <p></p>Upr=xbarGlu + s; <p></p><p></p>Interval = c(Lwr, Upr)
        <p></p>
        <p style="color:black; font-size: 12pt">
        The One-Sigma Interval for the Glucose attributes in the Pima Indian dataset is between the following two values :...
        <p></p>'
  output$text.11.2 <- renderUI({
    tags$div(
      HTML(sHTML_for_GlucoseCheb_Calc)
    )
  })
  
  
  
  # text output for Glucose Histogram and Chebyshev's rule
  sHTML_for_GlucoseCheb_Outl=
    '<p style="color:black; font-size: 12pt">
        <p></p>
        <p></p>
        <p></p>
        Chebyshevs rule, when applied to the list of values for the Glucose attribute in the Pima Indian dataset, 
        has identified the following list of outliers.. 
        <p></p><p></p>(Note the break in this sorted sequence as the values reflect outliers on either side of the one-sigma interval
        - list of outliers is seen best if the application is opened in a browser.)
        <p></p>'
  output$text.11.3 <- renderUI({
    tags$div(
      HTML(sHTML_for_GlucoseCheb_Outl)
    )
  })
  
  # text output for Central Measures of DBF
  sHTML_for_DBF_Mean=
       '<p></p>
        <p></p>
        <p></p>
        Continous Attribute : Diabetes Pedigree Function : Central Measure : MEAN'
  output$text.12.1 <- renderUI({
    tags$div(
      HTML(sHTML_for_DBF_Mean)
    )
  })
  
  # text output for Central Measures of DBF
  sHTML_for_DBF_Median=
    '<p></p>
        <p></p>
        <p></p>
        Continous Attribute : Diabetes Pedigree Function : Central Measure : MEDIAN'
  output$text.12.2 <- renderUI({
    tags$div(
      HTML(sHTML_for_DBF_Median)
    )
  })
  
  # text output for Central Measures of DBF
  sHTML_for_DBF_Mode=
    '<p></p>
        <p></p>
        <p></p>
        Continous Attribute : Diabetes Pedigree Function : Central Measure : MODE'
  output$text.12.3 <- renderUI({
    tags$div(
      HTML(sHTML_for_DBF_Mode)
    )
  })
  
  
  # text output for Variational Measures of DBF : RANGE
  sHTML_for_DBF_Range=
    '<p></p>
        <p></p>
        <p></p>
        Continous Attribute : Diabetes Pedigree Function : Variational Measure : RANGE'
  output$text.12.4 <- renderUI({
    tags$div(
      HTML(sHTML_for_DBF_Range)
    )
  })
  
  
  # text output for Variational Measures of DBF : VARIANCE
  sHTML_for_DBF_Var=
    '<p></p>
        <p></p>
        <p></p>
        Continous Attribute : Diabetes Pedigree Function : Variational Measure : VARIANCE'
  output$text.12.5 <- renderUI({
    tags$div(
      HTML(sHTML_for_DBF_Var)
    )
  })
  
  # text output for Variational Measures of DBF : STD DEVIATION
  sHTML_for_DBF_SD=
    '<p></p>
        <p></p>
        <p></p>
        Continous Attribute : Diabetes Pedigree Function : Variational Measure : STD DEVIATION'
  output$text.12.6 <- renderUI({
    tags$div(
      HTML(sHTML_for_DBF_SD)
    )
  })
  
}

shinyApp(ui, server)