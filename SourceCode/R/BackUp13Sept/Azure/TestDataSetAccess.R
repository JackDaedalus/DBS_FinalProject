library("AzureML")

ws <- workspace(
  id = "7375a43f744940748c691a78945b2588",
  auth = "hItFXmJqT2qV4rYUREXnnfy5ZfleIiXBiQB8q1bA1zfF53dcJJP4CwqXvgy2Wls/QpAhX0jEqzaLNWqjMfKDTQ==",
  api_endpoint = "https://europewest.studioapi.azureml.net"
)
ds <- download.datasets(
  dataset = ws,
  name = "CreditCard_Fraud_Dataset_2KRows_v1-1_July2020.csv"
)
head(ds)





