
library("curl")
library("httr")
library("rjson")

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


req = list(
  
  Inputs = list(
    
    
    "input1" = list(
      "ColumnNames" = list("MerchantCategory", "PinIndicator", "Fraud", "NonEMVTransactionsCount.cnt.day.present", "DomesticAuthCounter.cnt.day.present", "POSTerminalAttendedAuthCount.cnt.day.present", "DomesticAuthCount.cnt.hour1", "DomesticAuthCount.cnt.hour3", "DomesticAuthCount.cnt.hour4", "DomesticAuthCount.cnt.hour10", "DomesticAuthCount.cnt.hour15", "DomesticAuthCount.cnt.hour25", "EMVTransactionsCount.cnt.day.present", "CustomerNotPresentAuthCount.cnt.day.present", "OnlinePOSCountForever.cnt.present", "POS_Count.cnt.day.present", "OnlinePOSCount.cnt.day.present"),
      "Values" = list( list( "5812", "1", "0", "1", "1", "177", "1", "1", "1", "0", "0", "0", "0", "1", "1", "1", "1" ),  list( "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0" )  )
    )                ),
  GlobalParameters = setNames(fromJSON('{}'), character(0))
)

body = enc2utf8(toJSON(req))
api_key = "19Hvqviz3EkyUImSNxjNRO9KXviBuktyum9qMfsf57PYV8HLI7CC8b3VCZZLXc7PcYakF9+2hy5dJEcijKbK7w==" # Replace this with the API key for the web service
authz_hdr = paste('Bearer', api_key, sep=' ')

response = POST(url= "https://europewest.services.azureml.net/workspaces/7375a43f744940748c691a78945b2588/services/a68b0ed52e27489299cd3939b7dce0c8/execute?api-version=2.0&details=true",
                add_headers("Content-Type" = "application/json", "Authorization" = authz_hdr),
                body = body)

result = content(response, type="text", encoding="UTF-8")


printHttpResult(response, result)
