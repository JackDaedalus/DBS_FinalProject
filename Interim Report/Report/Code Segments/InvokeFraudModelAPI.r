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
req =  list(
    Inputs = list(
            "input1"= list(
                list(
                        'MerchantCategory' = "5571",
                        'PinIndicator' = "1",
                        'Fraud' = "0",
                        'NonEMVTransactionsCount.cnt.day.present' = "0",
                        'DomesticAuthCounter.cnt.day.present' = "1",
                        'POSTerminalAttendedAuthCount.cnt.day.present' = "1",
                        'DomesticAuthCount.cnt.hour1' = "1",
                        'DomesticAuthCount.cnt.hour3' = "1",
                        'DomesticAuthCount.cnt.hour4' = "1",
                        'DomesticAuthCount.cnt.hour10' = "1",
                        'DomesticAuthCount.cnt.hour15' = "1",
                        'DomesticAuthCount.cnt.hour25' = "1",
                        'EMVTransactionsCount.cnt.day.present' = "1",
                        'CustomerNotPresentAuthCount.cnt.day.present' = "0",
                        'OnlinePOSCountForever.cnt.present' = "71",
                        'POS_Count.cnt.day.present' = "1",
                        'OnlinePOSCount.cnt.day.present' = "1"
                    )
            )
        ),
        GlobalParameters = setNames(fromJSON('{}'), character(0))
)
body = enc2utf8(toJSON(req))
api_key = "abc123" # Replace this with the API key for the web service
authz_hdr = paste('Bearer', api_key, sep=' ')
response=POST(url = "https://europewest.services.azureml.net/workspaces/7375a43f744940748c691a78945b2588/services/a68b0ed52e27489299cd3939b7dce0c8/execute?api-version=2.0&format=swagger",
add_headers('Content-Type' = "application/json", 'Authorization' = authz_hdr),
body=body)
result = content(response, type="text", encoding="UTF-8")

printHttpResult(response, result)
