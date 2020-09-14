## Higher Diploma in Science in Data Analytics : Project : Module Code B8IT110
## Student Name : Ciaran Finnegan

## Student Number : 10524150

## September 2020



ReturnDSWithSelectedFeatures = function(){
  
  import(readr)
  import(data.table)
  
  
  ## Temporary local dataset for testing main Production Fraud Predictive Model
  
  dat2 <- fread("Azure/Exp6A_CCTrxns_7_Sample_Trxns.csv", 
                select = c(
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
                ))
  
  dat2.df <- as.data.frame(dat2)
  
  dat2.df
  
  
  
}



SelectColumsToHide = function(cc_Txns){
  
  # Restrict Display on single view of a selected credit card transaction
  # Done to improve on screen display and navigation
  
  
  # Choose Columns NOT to Display
  select_cols = c(
    'AccountSourceUniqueId',
    'MerchantCategory',
    'AcquirerRefId',
    'AuthId',
    'DvcVerificationCap',
    'DeviceCountryCode',
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
    'AlternatingCountrySwapCounter.cnt.day.past1')
  
  
  # Passed back list of columns are then used as a parameter to a datatableproxy function
  columns2hide <- match(select_cols, colnames(cc_Txns))
  
  
  
}