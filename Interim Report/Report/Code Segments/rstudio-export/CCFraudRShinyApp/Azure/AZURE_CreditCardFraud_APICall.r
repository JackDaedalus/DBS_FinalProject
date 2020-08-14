

PrintFraudModelResult = function(){
  
    import(tidyverse)
    import(readr)
    import(stats)
    import(curl)
    import(rjson)
    import(httr)
    
    
    MerchCat <- "4816"
    CCTxn <- read_csv("Azure/Sample_CC_Fraud_Txns.csv")
    

    requestFailed = function(response) {
      return (response$status_code >= 400)
    }
    
    printHttpResult = function(response, result) {
      if (requestFailed(response)) {
        print(paste("The request failed with status code:", response$status_code, sep=" "))
        
        # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
        print(response$headers)
      }
      
      print("Result:") 
      print(fromJSON(result))  
    }
    
    req =  list(
      Inputs = list(
        "input1"= list(
          list(
            ##'MerchantCategory' = "4816",
            'MerchantCategory' = MerchCat,
            'PinIndicator' = "0",
            'Fraud' = "0",
            'NonEMVTransactionsCount.cnt.day.present' = "76",
            'DomesticAuthCounter.cnt.day.present' = "3",
            'POSTerminalAttendedAuthCount.cnt.day.present' = "9",
            'DomesticAuthCount.cnt.hour1' = "3",
            'DomesticAuthCount.cnt.hour3' = "3",
            'DomesticAuthCount.cnt.hour4' = "3",
            'DomesticAuthCount.cnt.hour10' = "3",
            'DomesticAuthCount.cnt.hour15' = "3",
            'DomesticAuthCount.cnt.hour25' = "3",
            'EMVTransactionsCount.cnt.day.present' = "0",
            'CustomerNotPresentAuthCount.cnt.day.present' = "74",
            'OnlinePOSCountForever.cnt.present' = "-81",
            'POS_Count.cnt.day.present' = "0",
            'OnlinePOSCount.cnt.day.present' = "-76"
          )
        )
      ),
      GlobalParameters = setNames(fromJSON('{}'), character(0))
    )
    
    body = enc2utf8(toJSON(req))
    api_key = "19Hvqviz3EkyUImSNxjNRO9KXviBuktyum9qMfsf57PYV8HLI7CC8b3VCZZLXc7PcYakF9+2hy5dJEcijKbK7w==" # Replace this with the API key for the web service
    authz_hdr = paste('Bearer', api_key, sep=' ')
    
    response=POST(url = "https://europewest.services.azureml.net/workspaces/7375a43f744940748c691a78945b2588/services/a68b0ed52e27489299cd3939b7dce0c8/execute?api-version=2.0&format=swagger",
                  add_headers('Content-Type' = "application/json", 'Authorization' = authz_hdr),
                  body=body)
    
    result = content(response, type="text", encoding="UTF-8")
    
    printHttpResult(response, result)
    
    
}    


