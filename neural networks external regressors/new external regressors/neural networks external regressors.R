#first we create a dataframe with all the variables we consider relevant (or can be relevant) for estimating the output via the neural networks. 
library(readr)
library(dplyr)
fsi = read.csv("fsi.csv") #same problem as with gold and sp500; missing data from weekends
fsi = fsi[, c(1,2)]
require(tidyr)
fsi = fsi %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(as.Date("2012-12-26"), as.Date("2017-12-23"), 1))

colnames(fsi) = c("date", "fsi")
for (i in 1:nrow(fsi)) {
  if(is.na(fsi$fsi[i]) == FALSE) {
    next
  } else if (is.na(fsi$fsi[i]) == TRUE) {
    fsi$fsi[i] = fsi$fsi[i-1]
  } #we fill the NAs with the previous value; 
  #as the market is not active, the price does not change in these days. 
}

fsi = fsi %>%
  filter(date > "2013-01-01")

gold = read_csv("gold_usd.csv")
sp500 = read_csv("SP500.csv")

gold <- gold[seq(dim(gold)[1],1),]
gold = gold %>% 
  filter(Date > "2013-01-01" & Date < "2017-12-24")
sp500  = sp500 %>%
  filter(DATE > "2013-01-01" & DATE < "2017-12-24")
#we have a big problem here, as there are many days missing in the sp500 and gold prices. 

require(tidyr)
gold = gold %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(as.Date("2013-01-02"), as.Date("2017-12-23"), 1))

sp500 = sp500 %>%
  mutate(Date = as.Date(DATE)) %>%
  complete(Date = seq.Date(as.Date("2013-01-02"), as.Date("2017-12-23"), 1))

for (i in 1:nrow(gold)) {
  if(is.na(gold$Value[i]) == FALSE) {
    next
  } else if (is.na(gold$Value[i]) == TRUE) {
    gold$Value[i] = gold$Value[i-1]
  } #we fill the NAs with the previous value; 
  #as the market is not active, the price does not change in these days. 
}

for (i in 1:nrow(sp500)) {
  if(sp500$SP500[i] == "." | is.na(sp500$SP500[i]) == TRUE) {
    sp500$SP500[i] = sp500$SP500[i-1]
  } else if (sp500$SP500[i] != "." | is.na(sp500$SP500[i]) == FALSE) {
    next
  } #we fill the NAs with the previous value; 
  #as the market is not active, the price does not change in these days. 
}

gold = gold[ , 2]
sp = sp500[ , 3]
date = sp500[, 1]
df = data.frame(cbind(date, gold, sp))

trans_conf_time = read.csv("BCHAIN-ATRCT.csv")
hrate = read.csv("BCHAIN-HRATE.csv")
miners_rev = read.csv("BCHAIN-MIREV.csv")
tot_out_vol = read.csv("BCHAIN-TOUTV.csv")

trans_conf_time = trans_conf_time %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(as.Date("2013-01-02"), as.Date("2017-12-23"), 1))


hrate = hrate %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(as.Date("2013-01-02"), as.Date("2017-12-23"), 1))

miners_rev = miners_rev %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(as.Date("2013-01-02"), as.Date("2017-12-23"), 1))

tot_out_vol = tot_out_vol %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(as.Date("2013-01-02"), as.Date("2017-12-23"), 1))

df$tr_conf_time = trans_conf_time$Value
df$hrt = hrate$Value
df$mire = miners_rev$Value
df$tov = tot_out_vol$Value

colnames(df) = c("date", "gold", "sp", "tr_conf_time", "hrt", "mire", "tov")
#install.packages("timeSeries")
#library("timeSeries")
class(df$date)
#df = timeSeries(df)
#df_ts = timeSeries(data = df, object = df$date)
#df_scaled = scale(df_ts)

#class(df) = as.ts(df)
df2 = df[, 2:ncol(df)]


lr = function(x) {
  log_returns = diff(log(x), lag=1)
}
df2$sp = as.numeric(df2$sp)

g_r = scale(lr(df2$gold))
sp_r = scale(lr(df2$sp))
tr_r = scale(lr(df2$tr_conf_time))
hr_r = scale(lr(df2$hrt))
mire_r = scale(lr(df2$mire))
tov_r = scale(lr(df2$tov))

df_ret = data.frame(cbind(g_r, sp_r, tr_r, hr_r, mire_r, tov_r))



library(readr)
library(dplyr)
#btc = read_csv("btc_usd.csv") #more complete dataframe
btc = read_csv("btc_usd_2812.csv")
btc = data.frame(btc)
btc = btc[, 2:8] #we take out those variables for which we only have data from may 2017. 
colnames(btc) = c("date", "close", "high", "low", "open", "volumefrom", "volumeto")
btc2 = btc[order(as.Date(btc$date, format = "%Y-%m-%d")), ]
btc2 = btc2 %>% 
  filter( date > "2013-01-02" & date < "2017-12-24")

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
df_ret = df_ret[2:nrow(df_ret),]
colnames(df_ret) = c("gold", "sp", "tr_conf", "hrt", "mire", "tov")
arima_pred = c()
for(i in 200:1815) {
  arima = auto.arima(bitcoin.lr[1:i-1], xreg = df_ret[1:i-1, ])
  fore = forecast(object = arima, h = 1, xreg = df_ret[i, ])
  arima_pred = c(arima_pred, fore$mean)
}

prueba = data.frame(bitcoin.lr[200:1815])
prueba$pred = arima_pred
colnames(prueba) = c("btc", "pred")
require(ggplot2)
ggplot(data = prueba, aes(x = seq(1, 1616, 1))) +
  geom_line(aes(y = btc)) +
  geom_line(aes(y = pred, color = "red")) 

accuracy(f = prueba$pred, x = prueba$btc) #no esta nada mal; asi que vamos a incluir los valores calculados en este arima como un nodo mas de nuestra red neuronal. 




#input and target matrix for training and validation dataset.
input = matrix(c(rsi[200:1500], cci[200:1500],
                 macd[200:1500], will[200:1500], 
                 stochK[200:1500], stochD[200:1500],
                 arima_pred[1:1301]), nrow = 1301)

target = matrix(c(bitcoin.lr[201:1501]), nrow = 1301)

trainingdata = cbind(input, target)
colnames(trainingdata) = c("RSI", "CCI", "MACD", "WILL", 
                           "STOCHK", "STOCHD", "arima", "Return")

#install.packages("caret")
library(caret)

trainIndex = createDataPartition(bitcoin.lr[201:1501], 
                                 p = 0.9, list = FALSE)
bitcoin.train = trainingdata[trainIndex, ]
bitcoin.test = trainingdata[-trainIndex, ]

#before trying these nets, lets go for other ones:
# install library
#install.packages("neuralnet")

# load library
library(neuralnet)

# creating training and test set
#trainNN = scaled[index , ]
#testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD + arima,
               bitcoin.train, hidden = 7 , linear.output = F )

# plot neural network
plot(NN)





#install.packages("nnet")
library(nnet)
#let´s start with neural networks 
best.network = matrix(c(5, 0.5))
best.rmse = 1
for (i in 5:15) 
  for(j in 1:3) {
    bitcoin.fit = nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD + arima,
                       data = bitcoin.train, maxit = 1000000, size = i,
                       decay = 0.01 * j, linout = 1)
    bitcoin.predict = predict(bitcoin.fit, newdata = bitcoin.test)
    bitcoin.rmse = sqrt((mean(bitcoin.predict - bitcoin.lr[1374:1501])^2))
    if(bitcoin.rmse < best.rmse) {
      best.network[1,1] -> i 
      best.network[2,1] -> j
      best.rmse = bitcoin.rmse
    }
  }

inputTest = matrix(c(rsi[1501:1814], cci[1501:1814],
                     macd[1501:1814], will[1501:1814], stochK[1501:1814], 
                     stochD[1501:1814], arima_pred[1501:1814]), nrow = 314)
TargetTest = matrix(c(bitcoin.lr[1502:1815]), nrow = 314)
testdata = cbind(inputTest, TargetTest)
colnames(testdata) = c("RSI", "CCI", "MACD", "WILL", 
                       "STOCHK", "STOCHD","arima",  "Return")

bitcoin.fit = nnet(Return ~ RSI + CCI + MACD + WILL +
                     STOCHK + STOCHD + arima, data = trainingdata, maxit = 10000,
                   size = best.network[1,1], decay = 0.1*best.network[2,1],
                   linout = 1)
bitcoin.predict1 = predict(bitcoin.fit, newdata = testdata)
for ( i in 1:20) {
  bitcoin.fit = nnet(Return ~ RSI + CCI + MACD + WILL +
                       STOCHK + STOCHD + arima,  data = trainingdata, maxit = 1000,
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



