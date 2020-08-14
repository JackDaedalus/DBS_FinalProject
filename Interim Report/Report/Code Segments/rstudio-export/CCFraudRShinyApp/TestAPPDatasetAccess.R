

api_call <- modules::use("Azure")
# Read Azure Hosted Credit Card Fraud Dataset for Prototype
ds_cctxns = api_call$AZURE_CreditCardFraud_Download$download2KPrototypeCCTxns()
#head(ds_cctxns)

f <- ds_cctxns$Fraud

countf <- length(which(f == 1))
countf

countnf <- length(which(f == 0))
countnf