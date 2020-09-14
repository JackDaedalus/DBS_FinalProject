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
      "ColumnNames" = list("status_checking", "loan_duration", "credit_history", "purpose", "credit_amount", "savings_amt", "emp_years", "percent_disp_income", "status_sex", "other_debts", "residence_since", "property", "age", "other_installments", "housing_status", "num_credits", "job_type", "num_dependant", "own_telephone", "foreign_worker", "credit_risk"),
      "Values" = list( list( "A11", "9", "A34", "A43", "1169", "A65", "A75", "4", "A93", "A101", "4", "A121", "67", "A143", "A152", "2", "A173", "1", "A192", "A201", "1" ),  list( "value", "0", "value", "value", "0", "value", "value", "0", "value", "value", "0", "value", "0", "value", "value", "0", "value", "0", "value", "value", "0" )  )
    )                ),
  GlobalParameters = setNames(fromJSON('{}'), character(0))
)

body = enc2utf8(toJSON(req))
api_key = "88tLDb8AzRCmctCAEBJSSv76EhnYd1IFb/4UacFTnHfPE1Ct3jJzuFrlH91dl9KagtOrQzQyo79p/32UMgegbQ==" # Replace this with the API key for the web service
authz_hdr = paste('Bearer', api_key, sep=' ')

response = POST(url= "https://europewest.services.azureml.net/workspaces/7375a43f744940748c691a78945b2588/services/ae05093b7b3549b38e754302055c36f7/execute?api-version=2.0&details=true",
                add_headers("Content-Type" = "application/json", "Authorization" = authz_hdr),
                body = body)

result = content(response, type="text", encoding="UTF-8")


printHttpResult(response, result)


##print(result)
