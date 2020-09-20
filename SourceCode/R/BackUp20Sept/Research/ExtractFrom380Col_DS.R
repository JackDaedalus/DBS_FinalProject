library(readr)
library(data.table)


## Temporary local dataset for testing main Production Fraud Predictive Model

dat2 <- fread("CCFraudRShinyApp/Azure/Exp6A_CCTrxns_7_Sample_Trxns.csv", 
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
dat2

