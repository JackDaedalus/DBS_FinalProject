## Higher Diploma in Science in Data Analytics : Project : Module Code B8IT110
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## September 2020


# Functions to calculate values for display on screen for dataset visualizations
# This code is being written in a separate R file to make the overall project 
# code more readable. 

# The 'app.r' file for the Shiny R application is a large file and these functions
# are being written here and invoked directly from the code in 'app.r'.



tstTableMat = function(ds) {
  

  frd_custPres <- table(ds$Fraud, ds$CustomerPresentIndicator)
  frd_custPres


}  


generateFraudBlananceChart = function(ds) {
  
  # Generate a count by Fraud Outcome in dataset   ##'Fraud' is output variable
  values <- ds %>%
    group_by(Fraud) %>%
    summarize(count = n())
  
  # Set Colours 
  colours <- c('rgb(27, 158, 119)', 'rgb(217, 95, 2)')#"#1B9E77" "#D95F02"
  
  labels = c('Non-Fraud','Fraud')
  
  p1 <- plot_ly(data = values,type='pie', 
          labels=labels, 
          values=~count,
          text = ~count,
          marker = list(colors = colours,
                        line = list(color = '#FFFFFF', width = 1)))
  
  p1
  
} 



# Generate Credit Card Trxn Amount BoxPlot against Fraud Outcome and remove outliers  ##Fraud' is output variable
frdBoxPlot_NoOutliers = function(ds) {
  
  p0 = ggplot(ds, aes(x = Fraud, y = AmountOrig, col = Fraud)) +
    geom_boxplot(outlier.shape = NA) +
    ylab("Credit Card Trxn Amount") +
    scale_color_manual(values = c("red", "black")) +
    scale_fill_manual(values = c("red", "black"))
  
  # Rescale the Box Plot to Remove the outliers - only focus on quartiles and whiskers
  sts <- boxplot.stats(ds$AmountOrig)$stats
  p1 = p0 + coord_cartesian(ylim = c(sts[2]/2,max(sts)*1.75))
  p1
  
}  


# Generate Credit Card Trxn Amount BoxPlot against Fraud Outcome and include outliers  ##Fraud' is output variable
frdBoxPlot_WithOutliers = function(ds) {
  
  pl1 <- ggplot(ds, aes(x = Fraud, y = AmountOrig, col = Fraud)) +
    geom_boxplot(alpha = 0.2,
                 outlier.shape=8,
                 outlier.size=3) +
    ylab("Credit Card Trxn Amount") +
    scale_color_manual(values = c("red", "black")) +
    scale_fill_manual(values = c("red", "black"))
  
  pl1
  
}  



## Return a Four Fold Style 2-Dimensional Matrix Table showing ratio of Fraud broken
## down by those transactions where a PIN was or was not used.
fGetPlot_PinInd = function(ds) {
  

  frd_pin <- table(ds$Fraud, ds$PinIndicator)
  
  fourfoldplot(frd_pin, color = c("#CC6666", "#99CC99"),
               conf.level = 0, margin = 1, main = "Fraud vs PIN Used")
  

}


## Return a Four Fold Style 2-Dimensional Matrix Table showing ratio of Fraud broken
## down by those transactions where a PIN was or was not used.
fGetPlot_EComm = function(ds) {
  
  
  frd_eComm <- table(ds$Fraud, ds$ECommerceFlag)
  
  fourfoldplot(frd_eComm, color = c("#CC6666", "#99CC99"),
               conf.level = 0, margin = 1, main = "Fraud vs ECommerce Flag")
  
  
}



## Return a datatable based on Fraud and CustomerPresentIndicator. 
## The output will be sorted on Fraud/non-Fraud that shows a breakdown of fraud based
## on whether the customer was physically present at the credit card transaction 
fGetPlot_CustPres = function(ds) {
  
  frd_custPres <- table(ds$Fraud, ds$CustomerPresentIndicator)
  frd_custPres
  
}


## Generate and return a 100% stacked bar chart that shows the percentage of Fraud/non-Fraud
## across the different geographical regions in the Credit Card dataset
fGetPlot_frdGeo = function(ds) {
 

  ds$Fraud <- as.factor(ds$Fraud)
  levels(ds$Fraud) <- c("Not Fraud", "Fraud")   # Fraud
  
  
  # Get labels
  frdPercentData <- ds  %>% group_by(DeviceCountryCode) %>% count(Fraud) %>%
    mutate(ratio=scales::percent(n/sum(n)))
  
  
  
  p1 <- ggplot(ds, aes(x=DeviceCountryCode, fill = factor(Fraud))) +
    geom_bar(color = "black", position = "fill") +
    geom_text(data = frdPercentData, aes(y=n, label=ratio),
              position = position_fill(vjust = 0.5)) +
    scale_y_continuous(labels = percent_format()) +
    labs(y="Percentage Fraud") +
    scale_x_discrete("Geographical Regions") +
    ggtitle("Credit Card Fraud Breakdown Per Geographical Region") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom", legend.direction = "horizontal")
  
  p1
   
}  

