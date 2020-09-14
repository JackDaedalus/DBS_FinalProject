

api_call <- modules::use("Azure")
ui_data <- modules::use("UserApp")

# # Read Azure Hosted Credit Card Fraud Dataset for Prototype
# ds_cctxns = api_call$AZURE_CreditCardFraud_Download$download2KPrototypeCCTxns()
## Temporary local dataset for testing main Production Fraud Predictive Model
cc_Txns = ui_data$DATA_FileSelection$ReturnDSWithSelectedFeatures()




## Function to parse CC Trxn Row into parameters for API call
get.arg.list.from.cctrxn <- function(){
  
  # Prepare parameter list of given credit card transaction
  chosen_cc_trxns <- cc_Txns
  len_list <- length(chosen_cc_trxns)
  arg.list <- list()
  
  # Start reading cc trxn record on first entry in row
  i <- 1
  
  # arg.list <- append(arg.list, chosen_cc_trxns[1,i])

  while(i<(len_list+1)){  # This Reflects the number of attribute entries in the cc_trxn record  
     
     arg.list <- append(arg.list, chosen_cc_trxns[1,i])
     i <- i + 1
     
   }
  
  return(arg.list)
  
}




  
# Call function to prepare list of attributes for the API call to the predictive Fraud model
api.arg.list.score <- get.arg.list.from.cctrxn()
  
# Indicate that only the model score is required
api.arg.list.score <- append(api.arg.list.score, "Score_Only")
  
# Pass Parameters for API Call - Full Production Model
do.call(api_call$AZURE_9C_CCFraud_APICall$Print9CFraudModelResult, api.arg.list.score)
  




