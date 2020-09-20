
Print9CFraudModelResult = function(
                                  Fraud,      #  'Fraud', 
                                  AccSrcUID,  #  'AccountSourceUniqueId',
                                  CrdSrcRID,  #  'CardSourceRefId',
                                  MerchCat,   #  'MerchantCategory',
                                  AcqRID,     #  'AcquirerRefId',
                                  AuthId,     #  'AuthId',
                                  Amount,     #  'AmountOrig',
                                  CardType,   #  'CardType',
                                  DvcVrfCap,  #  'DvcVerificationCap',
                                  CustPresInd,#  'CustomerPresentIndicator',
                                  PosTrmAttd, #  'PosTerminalAttended',
                                  DeviceZone, #  'DeviceZone',
                                  DeviceCntCd,#  'DeviceCountryCode',
                                  ECommerceFlag, #  'ECommerceFlag',
                                  PinIndicator,  #  'PinIndicator',
                                  HRkPOSCtHPres, #  'HighRiskPOSCnt.cnt.hour.present',
                                  FPmpCntDtTot,  #  'FuelPumpCount.cnt.day.total',
                                  FPmpCntDtPres, #  'FuelPumpCount.cnt.day.present',
                                  NtECommAtAmtP3,#  'NotECommerceAuthAmount.acc.day.past3',
                                  NEMVTrxnCntDy29, #  'NonEMVTransactionsCount.cnt.day.past29',
                                  PosTrmAttdACntDy3, #'POSTerminalAttendedAuthCount.cnt.day.past3',
                                  DomAthCnt1,      #  'DomesticAuthCount.cnt.hour1',
                                  NtECommAtAmtPres,#  'NotECommerceAuthCount.cnt.day.present',
                                  EMVTxCntPres,    #  'EMVTransactionsCount.cnt.day.present',
                                  EMVTxCntDy3,     #  'EMVTransactionsCount.cnt.day.past3',
                                  POSCntPres,      #  'POS_Count.cnt.day.present',
                                  EMVTxCntDy1,     #  'EMVTransactionsAcc.acc.day.past1',
                                  AltCntSpCntDy1,  # 'AlternatingCountrySwapCounter.cnt.day.past1'
                                  ReqResp          # Control flag to parse output
                                  )
{

    import(stats)
    import(curl)
    import(rjson)
    import(httr)


    
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
            'AccountSourceUniqueId' = AccSrcUID,
            'CardSourceRefId' = CrdSrcRID,
            'MerchantCategory' = MerchCat,
            'AcquirerRefId' = AcqRID,
            'AuthId' = AuthId,
            'AmountOrig' = Amount,
            'CardType' = CardType,
            'DvcVerificationCap' = DvcVrfCap,
            'CustomerPresentIndicator' = CustPresInd,
            'PosTerminalAttended' = PosTrmAttd,
            'DeviceZone' = DeviceZone,
            'DeviceCountryCode' = DeviceCntCd,
            'ECommerceFlag' = ECommerceFlag,
            'PinIndicator' = PinIndicator,
            'Fraud' = "0", # Not used in modeling process
            'HighRiskPOSCnt.cnt.hour.present' = HRkPOSCtHPres,
            'FuelPumpCount.cnt.day.total' = FPmpCntDtTot,
            'FuelPumpCount.cnt.day.present' = FPmpCntDtPres,
            'NotECommerceAuthAmount.acc.day.past3' = NtECommAtAmtP3,
            'NonEMVTransactionsCount.cnt.day.past29' = NEMVTrxnCntDy29,
            'POSTerminalAttendedAuthCount.cnt.day.past3' = PosTrmAttdACntDy3,
            'DomesticAuthCount.cnt.hour1' = DomAthCnt1,
            'NotECommerceAuthCount.cnt.day.present' = NtECommAtAmtPres,
            'EMVTransactionsCount.cnt.day.present' = EMVTxCntPres,
            'EMVTransactionsCount.cnt.day.past3' = EMVTxCntDy3,
            'POS_Count.cnt.day.present' = POSCntPres,
            'EMVTransactionsAcc.acc.day.past1' = EMVTxCntDy1,
            'AlternatingCountrySwapCounter.cnt.day.past1' = AltCntSpCntDy1
          )
        )
      ),
      GlobalParameters = setNames(fromJSON('{}'), character(0))
    )
    
    body = enc2utf8(toJSON(req))
    api_key = "PnzViKK7tw+34LsnpD5B4PwPpUEgEDy5wm3AVoK2OYSRMtA+Kp7aLqgKes1x8Zru+ml1JVJQwpUrEcjGtbYEsQ==" # Replace this with the API key for the web service
    authz_hdr = paste('Bearer', api_key, sep=' ')
    
    response=POST(url = "https://europewest.services.azureml.net/workspaces/7375a43f744940748c691a78945b2588/services/97919581182143709662bcdb1fb4eeb3/execute?api-version=2.0&format=swagger",
                  add_headers('Content-Type' = "application/json", 'Authorization' = authz_hdr),
                  body=body)
    
    result = content(response, type="text", encoding="UTF-8")
    
    ## Further Parsing Response from API call to decompose/extract the score result
    get2json<- content(response, as = "parsed")
    parse2json<- (toJSON(get2json))
    data1 <- fromJSON(parse2json)
    
    # Obtain binary result from Fraud Scoring Model
    scoreResult <- data1$Results$output1[[1]]$`Scored Labels`
    
    # Obtain Card Reference from Fraud Scoring Model - just used to enrichen the display
    cardRef <- data1$Results$output1[[1]]$CardSourceRefId
    
    # Obtain the Score Probability from the Model Scoring Output and 
    # format the value for display by reducing decimal points
    scoreProbability_String <- data1$Results$output1[[1]]$`Scored Probabilities`
    scoreProbability_Numeric <- as.numeric(scoreProbability_String)
    scoreProbability <- format(scoreProbability_Numeric, digits=4,nsmall=5)
    
    
    # Vary the text response based on calling parameter - different on screen buttons return separate levels of detail
    if (ReqResp == "Score_Only") {
      

      # Flag if the model is predicting fraud based on the generated 'score'
      if (scoreResult == "1") {
        
        testResult <- "It's FRAUD! Model 9C Says No!"
        
      } else {
        
        testResult <- "It's Not Fraud! Model 9C Says Yay!"
        
      }
      
      # Return Message Text to indicate if Fraud predicted or not.
      cat("\n","Card Ref No: ",cardRef,"\n","\n", testResult, "\n", "\n", "Score for Probability of Fraud is : ", scoreProbability)
    }
    
    else {
      
      # Print the entire response message
      printHttpResult(response, result)
      
    }
    
    
}      
