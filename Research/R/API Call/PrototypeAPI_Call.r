

PrintFraudModelResult = function(MerchCat,         # 1st param for API
                                 Pin,              # 2nd param for API
                                 Fraud,            # 3rd param for API - Not Used
                                 NEMVTxCnt,        # 4th param for API
                                 DomAthCnt,        # 5th param for API
                                 POSTmAtAthCnt,    # 6th param for API
                                 DomAthCnt1,       # 7th param for API
                                 DomAthCnt3,       # 8th param for API
                                 DomAthCnt4,       # 9th param for API
                                 DomAthCnt10,      # 10th param for API
                                 DomAthCnt15,      # 11th param for API
                                 DomAthCnt25,      # 12th param for API
                                 EMVTxCnt,         # 13th param for API
                                 CustNPresAthCnt,  # 14th param for API
                                 OLPOSCnt4Evr,     # 15th param for API
                                 POSCnt,           # 16th param for API
                                 OLPOSCnt,         # 17th param for API
                                 ReqResp           # Control flag to parse output
                                 )
                                {
  
    ##import(tidyverse)
    ##import(readr)
    import(stats)
    import(curl)
    import(rjson)
    import(httr)
    
    
    #MerchCat <- arg.list[1]
    ##CCTxn <- read_csv("Azure/Sample_CC_Fraud_Txns.csv")
    

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
             #'MerchantCategory' = "4816",
            'MerchantCategory' = MerchCat,
             #'PinIndicator' = "0",
            'PinIndicator' = Pin,
            'Fraud' = "0",
             #'NonEMVTransactionsCount.cnt.day.present' = "76",
            'NonEMVTransactionsCount.cnt.day.present' = NEMVTxCnt,
             #'DomesticAuthCounter.cnt.day.present' = "3",
            'DomesticAuthCounter.cnt.day.present' = DomAthCnt,
             #'POSTerminalAttendedAuthCount.cnt.day.present' = "9",
            'POSTerminalAttendedAuthCount.cnt.day.present' = POSTmAtAthCnt,
             #'DomesticAuthCount.cnt.hour1' = "3",
            'DomesticAuthCount.cnt.hour1' = DomAthCnt1,
             #'DomesticAuthCount.cnt.hour3' = "3",
            'DomesticAuthCount.cnt.hour3' = DomAthCnt3,
             #'DomesticAuthCount.cnt.hour4' = "3",
            'DomesticAuthCount.cnt.hour4' = DomAthCnt4,
             #'DomesticAuthCount.cnt.hour10' = "3",
            'DomesticAuthCount.cnt.hour10' = DomAthCnt10,
             #'DomesticAuthCount.cnt.hour15' = "3",
            'DomesticAuthCount.cnt.hour15' = DomAthCnt15,
             #'DomesticAuthCount.cnt.hour25' = "3",
            'DomesticAuthCount.cnt.hour25' = DomAthCnt25,
             #'EMVTransactionsCount.cnt.day.present' = "0",
            'EMVTransactionsCount.cnt.day.present' = EMVTxCnt,
             #'CustomerNotPresentAuthCount.cnt.day.present' = "74",
            'CustomerNotPresentAuthCount.cnt.day.present' = CustNPresAthCnt,
             #'OnlinePOSCountForever.cnt.present' = "-81",
            'OnlinePOSCountForever.cnt.present' = OLPOSCnt4Evr,
             #'POS_Count.cnt.day.present' = "0",
            'POS_Count.cnt.day.present' = POSCnt,
             #'OnlinePOSCount.cnt.day.present' = "-76"
            'OnlinePOSCount.cnt.day.present' = OLPOSCnt
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
    
    
    # Vary the text response based on calling parameter - different on screen buttons return separate levels of detail
    if (ReqResp == "Score_Only") {
      
      ## Further Parsing Response from API call to decompose/extract the score result
      get2json<- content(response, as = "parsed")
      parse2json<- (toJSON(get2json))
      data1 <- fromJSON(parse2json)
      scoreResult <- data1$Results$output1[[1]]$`Scored Labels`
      
      # Flag if the model is predicting fraud based on the generated 'score'
      if (scoreResult == "1") {
        
        testResult <- "It's FRAUD!"
        
      } else {
        
        testResult <- "It's Not Fraud!"
        
      }
      # Return Message Text to indicate if Fraud predicted or not.
      print(testResult)
    }
    
    else {
    
      # Print the entire response message
      printHttpResult(response, result)
      
    }
    
    
}    


