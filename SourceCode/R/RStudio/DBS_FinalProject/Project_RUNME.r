## Higher Diploma in Science in Data Analytics : Project : Module Code B8IT110 : March 2019 Intake - Friday/Saturday Class
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## July 2020

## This is the source code for the Shiny Semantic Dashbaord App that has two functions;

## 1 - To display a visual representation of my Credit Card Fraud dataset 
## 2 - Invoke an API call to access a Production deployment of an Azure hosted model for fraud prediction, training on this dataset 

## Set Working Directory Accordingly



# This install.packages line is only included to assist in the first run. If these packages are already installed then it is not required.
# install.packages(c("dplyr", "DT", "ggcorrplot", "ggplot2", "plotly", "reshape2", "semantic.dashboard", "shinythemes"))

# If there are issues runing the Shiny App, navidate to the \CCFraudRShinyApp folder and install packages from there.

# This command will launch the 'app.r' file in the CCFraudRShinyApp sub folder. 

runApp("CCFraudRShinyApp")

