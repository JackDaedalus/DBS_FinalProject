# Testing an Auto Price Prediction API for Final Project : Azure Tutorial Preparation
# Ciaran Finnegan : Student No. 10524150 : July 2020


library("RCurl")
library("rjson")

# Accept SSL certificates issued by public Certificate Authorities
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

h = basicTextGatherer()
hdr = basicHeaderGatherer()

req =  list(
  Inputs = list(
    "WebServiceInput0"= list(
      list(
        'symboling' = "3",
        'normalized-losses' = "1",
        'make' = "alfa-romero",
        'fuel-type' = "gas",
        'aspiration' = "std",
        'num-of-doors' = "two",
        'body-style' = "convertible",
        'drive-wheels' = "rwd",
        'engine-location' = "front",
        'wheel-base' = "108.6",
        'length' = "168.8",
        'width' = "74.1",
        'height' = "48.8",
        'curb-weight' = "2548",
        'engine-type' = "dohc",
        'num-of-cylinders' = "four",
        'engine-size' = "130",
        'fuel-system' = "mpfi",
        'bore' = "3.47",
        'stroke' = "2.68",
        'compression-ratio' = "9",
        'horsepower' = "111",
        'peak-rpm' = "5000",
        'city-mpg' = "33",
        'highway-mpg' = "49",
        'price' = "13495"
      )
    )
  ),
  
  GlobalParameters = setNames(fromJSON('{}'), character(0))
  
  
)

body = enc2utf8(toJSON(req))
api_key = "RPxaMIm7mPaqd4ku2zuegiM8bMLQPGdm" # Replace this with the API key for the web service
authz_hdr = paste('Bearer', api_key, sep=' ')

h$reset()
curlPerform(url = "http://23.97.224.187:80/api/v1/service/dbs-auto-price-model---2--july-2/score",
            httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
            postfields=body,
            writefunction = h$update,
            headerfunction = hdr$update,
            verbose = TRUE
)

headers = hdr$value()
httpStatus = headers["status"]
if (httpStatus >= 400)
{
  print(paste("The request failed with status code:", httpStatus, sep=" "))
  
  # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
  print(headers)
}

print("Result:")
result = h$value()
print(fromJSON(result))