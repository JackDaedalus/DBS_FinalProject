##devtools::install_github("RevolutionAnalytics/AzureML")
##library("AzureML")

##install.packages("AzureML")
##library(AzureML)

downloadNewCreditCardTxns = function(){

    import("AzureML")
    
    ws <- workspace(
      id = "7375a43f744940748c691a78945b2588",
      auth = "hItFXmJqT2qV4rYUREXnnfy5ZfleIiXBiQB8q1bA1zfF53dcJJP4CwqXvgy2Wls/QpAhX0jEqzaLNWqjMfKDTQ==",
      api_endpoint = "https://europewest.studioapi.azureml.net"
    )
    ds <- download.datasets(
      dataset = ws,
      name = "CreditCard_Fraud_Dataset_NewRows_SmallSet_v1-0_July2020.csv"
    )
    
    ##ds_ret = head(ds)
    
    return(ds)
    
    
}  



download2KPrototypeCCTxns  = function() {
  
  import("AzureML")
  
  ws <- workspace(
    id = "7375a43f744940748c691a78945b2588",
    auth = "hItFXmJqT2qV4rYUREXnnfy5ZfleIiXBiQB8q1bA1zfF53dcJJP4CwqXvgy2Wls/QpAhX0jEqzaLNWqjMfKDTQ==",
    api_endpoint = "https://europewest.studioapi.azureml.net"
  )
  ds <- download.datasets(
    dataset = ws,
    name = "CreditCard_Fraud_Dataset_2KRows_v1-1_July2020.csv"
  )
  
  return(ds)
  
  
  
  
}
