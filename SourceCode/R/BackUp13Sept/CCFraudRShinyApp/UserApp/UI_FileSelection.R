## Higher Diploma in Science in Data Analytics : Project : Module Code B8IT110
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## September 2020


TestFunctionReturn = function(){
  
  tst <- "Test Display"
  tst
  
}


funcExtractSelectedFeaturesfromFile = function(input_ds) {

  # Start by converting into a tibble data frame (tbl_df) for easier data analysis
  cc_data <- as_tibble(input_ds)  
  
  # Only select the important features from the file
  cc_data %>% select(one_of(c(
      'Fraud', 
      'AccountSourceUniqueId',
      'CardSourceRefId',
      'MerchantCategory',
      'AcquirerRefId',
      'AuthId',
      'AmountOrig',
      'CardType',
      'DvcVerificationCap',
      'CustomerPresentIndicator',
      'PosTerminalAttended',
      'DeviceZone',
      'DeviceCountryCode',
      'ECommerceFlag',
      'PinIndicator',
      'HighRiskPOSCnt.cnt.hour.present',
      'FuelPumpCount.cnt.day.total',
      'FuelPumpCount.cnt.day.present',
      'NotECommerceAuthAmount.acc.day.past3',
      'NonEMVTransactionsCount.cnt.day.past29',
      'POSTerminalAttendedAuthCount.cnt.day.past3',
      'DomesticAuthCount.cnt.hour1',
      'NotECommerceAuthCount.cnt.day.present',
      'EMVTransactionsCount.cnt.day.present',
      'EMVTransactionsCount.cnt.day.past3',
      'POS_Count.cnt.day.present',
      'EMVTransactionsAcc.acc.day.past1',
      'AlternatingCountrySwapCounter.cnt.day.past1'
    )))
  
  dat2.df <- as.data.frame(cc_data)
  
  dat2.df

}



# Module UI function
csvFileUI <- function(id, label = "\n") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Display Credit Card Transaction File Headings\n"),

  )
}



# Module server function
csvFileServer <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # The user's data, parsed into a data frame
      dataframe <- reactive({
        read.csv(userFile()$datapath,
                 header = input$heading,
                 quote = input$quote,
                 stringsAsFactors = stringsAsFactors)
      })
      
      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      # Only extract the required features from the new credit card transaction file
      cc_trxn_file_ds <- funcExtractSelectedFeaturesfromFile(dataframe)
      
      # Return the reactive that yields the data frame
      return(cc_trxn_file_ds)  #return(dataframe)
    }
  )    
}

