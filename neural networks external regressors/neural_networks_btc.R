#NEURAL NETWORKS PREDICTION.2
#install.packages("readr")
library(readr)
#btc = read_csv("btc_usd.csv") #more complete dataframe
btc = read_csv("btc_usd_2812.csv")
btc = data.frame(btc)
btc = btc[, 2:ncol(btc)]
colnames(btc) = c("date", "close", "high", "low", "open", "volumefrom", "volumeto")
btc2 = btc[order(as.Date(btc$date, format = "%Y-%m-%d")), ]
price = btc2$close
HLC = matrix(c(btc2$high, btc2$low, btc2$close), 
             nrow = length(btc2$high) )
library("TTR")
library(forecast)
bitcoin.lr = diff(log(price))

rsi = RSI(price)
macd = MACD(price)
macd= macd[ , 1]
will = williamsAD(HLC)
cci = CCI(HLC)
STOCH = stoch(HLC)
stochK = STOCH[,1]
stochD = STOCH[ , 1]
arima = auto.arima(price)




#input and target matrix for training and validation dataset.
input = matrix(c(rsi[600:2600], cci[600:2600],
                 macd[600:2600], will[600:2600], 
                 stochK[600:2600], stochD[600:2600],
                 arima[600:2600]), nrow = 2001)

target = matrix(c(bitcoin.lr[601:2601]), nrow = 2001)

trainingdata = cbind(input, target)
colnames(trainingdata) = c("RSI", "CCI", "MACD", "WILL", 
                           "STOCHK", "STOCHD", "arima", "Return")

#install.packages("caret")
library(caret)

trainIndex = createDataPartition(bitcoin.lr[601:2601], 
                                 p = 0.9, list = FALSE)
bitcoin.train = trainingdata[trainIndex, ]
bitcoin.test = trainingdata[-trainIndex, ]

#install.packages("nnet")
library(nnet)
#let´s start with neural networks 
best.network = matrix(c(5, 0.5))
best.rmse = 1
for (i in 5:15) 
  for(j in 1:3) {
    bitcoin.fit = nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD,
                       data = bitcoin.train, maxit = 10000, size = i,
                       decay = 0.01 * j, linout = 1)
    bitcoin.predict = predict(bitcoin.fit, newdata = bitcoin.test)
    bitcoin.rmse = sqrt((mean(bitcoin.predict - bitcoin.lr[1802:2001])^2))
    if(bitcoin.rmse < best.rmse) {
      best.network[1,1] -> i 
      best.network[2,1] -> j
      best.rmse = bitcoin.rmse
    }
  }

inputTest = matrix(c(rsi[1901:1930], cci[1901:1930],
                     macd[1901:1930], will[1901:1930], stochK[1901:1930], 
                     stochD[1901:1930]), nrow = 30)
TargetTest = matrix(c(bitcoin.lr[1902:1931]), nrow = 30)
testdata = cbind(inputTest, TargetTest)
colnames(testdata) = c("RSI", "CCI", "MACD", "WILL", 
                       "STOCHK", "STOCHD", "Return")

bitcoin.fit = nnet(Return ~ RSI + CCI + MACD + WILL +
                     STOCHK + STOCHD, data = trainingdata, maxit = 1000,
                   size = best.network[1,1], decay = 0.1*best.network[2,1],
                   linout = 1)
bitcoin.predict1 = predict(bitcoin.fit, newdata = testdata)
for ( i in 1:20) {
  bitcoin.fit = nnet(Return ~ RSI + CCI + MACD + WILL +
                       STOCHK + STOCHD,  data = trainingdata, maxit = 1000,
                     size = best.network[1,1], decay = 0.1*best.network[2,1],
                     linout = 1)
  
  bitcoin.predict = predict(bitcoin.fit, newdata = testdata)
  bitcoin.predict1 = (bitcoin.predict1 + bitcoin.predict) / 2
  
}

#calculate the result of the buy-and-hold benchmark strategy and neural network on the dataset. 

money = money2 = matrix(0,31)
money[1,1] = money2[1,1] = 100
for (i in 2:31) {
  direction1 = ifelse(bitcoin.predict[i-1] < 0, -1, 1)
  direction2 = ifelse(TargetTest[i-1] < 0, -1, 1)
  money[i,1] = ifelse((direction1 - direction2) == 0,
                      money[i-1,1] * (1+abs(TargetTest[i-1])),
                      money[i-1,1]*(1-abs(TargetTest[i-1])))
  money2[i,1] = 100*(price[1401 + i -1] / price[1401]) 
}

x = 1:31
matplot(cbind(money, money2), type = "l", xaxt = "n",
        ylab = "", col = c("black", "grey"), lty = 1)
legend("topleft", legend = c("Neural network", "Benchmark"),
       pch = 19, col = c("black", "grey"))
axis(1, at = c(1, 10, 20, 30), 
     lab = c("2014-05-17", "2014-05-27", "2014-06-06", "2014-06-16"))
box()
mtext(side= 1, "Test dataset", line = 2)
mtext(side = 2, "Investment Value", line = 2)



#let´s see if there is any relationship among the google search "bitcoin" and the btc price
require(readr)
search = read_csv("bitcoin_search_90d.csv")
require(dplyr)
btcsearch = btc[ ,1:2] %>%
  filter(date >= "2017-09-04")
btcsearch$search = search[ ,2]
ggplot(btcsearch, aes(x = date)) +
  geom_line(aes(y = scale(close))) +
  geom_line(aes(y = scale(search), colour = "red"))




