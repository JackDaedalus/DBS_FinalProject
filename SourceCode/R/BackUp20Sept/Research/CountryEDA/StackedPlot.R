## Test 100% Stacked Plot

  

library(ggplot2) # loads ggplot2  
library(reshape2)
library(scales)

# frdCntReport <- read.csv("CreditCard_Fraud_CountryCode_Cleaned_Dataset_EDA_25KRows_Sept_v1_2020.csv")
# 
# # Get labels
# frdPercentData <- frdCntReport  %>% group_by(DeviceCountryCode) %>% count(Fraud) %>%
#   mutate(ratio=scales::percent(n/sum(n)))
# 
# 
# 
# ggplot(frdCntReport, aes(x=DeviceCountryCode, fill = factor(Fraud))) +
#   geom_bar(position = "fill") +
#   geom_text(data = frdPercentData, aes(y=n, label=ratio),
#             position = position_fill(vjust = 0.5))
  


frdCntReport <- read.csv("CreditCard_Fraud_CountryCode_Cleaned_Dataset_EDA_25KRows_Sept_v1_2020.csv")

frdCntReport$Fraud <- as.factor(frdCntReport$Fraud)
levels(frdCntReport$Fraud) <- c("Not Fraud", "Fraud")   # Fraud


# Get labels
frdPercentData <- frdCntReport  %>% group_by(DeviceCountryCode) %>% count(Fraud) %>%
  mutate(ratio=scales::percent(n/sum(n)))



ggplot(frdCntReport, aes(x=DeviceCountryCode, fill = factor(Fraud))) +
  geom_bar(color = "black", position = "fill") +
  geom_text(data = frdPercentData, aes(y=n, label=ratio),
            position = position_fill(vjust = 0.5)) +
  scale_y_continuous(labels = percent_format()) +
  labs(y="Percentage Fraud") +
  scale_x_discrete("Geogrpahical Regions") +
  ggtitle("Credit Card Fraud Breakdown Per Geographical Region") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.direction = "horizontal")
