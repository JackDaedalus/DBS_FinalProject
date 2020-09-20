
##devtools::install_github("RevolutionAnalytics/AzureML")
##library("AzureML")

##install.packages("AzureML")
##library(AzureML)


## Higher Diploma in Science in Data Analytics : Project : Module Code B8IT110
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## August / September 2020


## These routines directly read credit card transaction datasets that are stored
## in Azure. This allows for greater ease of deployment of the application and
## more effective automatic access to the datasets 



## This function returns the credit card transaction dataset that was used for the
## creation of the final trained predictive model for fraudulent credit card transactions
download25KProductionCCTxns= function(){
  
  import("AzureML")
  

  ws <- workspace(
    id = "7375a43f744940748c691a78945b2588",
    auth = "hItFXmJqT2qV4rYUREXnnfy5ZfleIiXBiQB8q1bA1zfF53dcJJP4CwqXvgy2Wls/QpAhX0jEqzaLNWqjMfKDTQ==",
    api_endpoint = "https://europewest.studioapi.azureml.net"
  )
  ds <- download.datasets(
    dataset = ws,
    name = "CreditCard_Fraud_Cleaned_Dataset_EDA_25KRows_Sept_v1_2020.csv",
    fill = TRUE
  )
  
  return(ds)
  
  
}



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
    name = "CreditCard_Fraud_Dataset_2KRows_v1-1_July2020.csv",
    fill = TRUE
  )
  
  return(ds)
  
  
  
  
}



downloadDevCntryCdCCTxns = function() {
  
  import("AzureML")
  
  
  ws <- workspace(
    id = "7375a43f744940748c691a78945b2588",
    auth = "hItFXmJqT2qV4rYUREXnnfy5ZfleIiXBiQB8q1bA1zfF53dcJJP4CwqXvgy2Wls/QpAhX0jEqzaLNWqjMfKDTQ==",
    api_endpoint = "https://europewest.studioapi.azureml.net"
  )
  ds <- download.datasets(
    dataset = ws,
    name = "CreditCard_Fraud_CountryCode_Cleaned_Dataset_EDA_25KRows_Sept_v1_2020.csv"
  )
  
  
  return(ds)
  
}


