## Test Module
library(modules)

api_call <- modules::use("Azure")

api_call$invoke$fraud_model()

