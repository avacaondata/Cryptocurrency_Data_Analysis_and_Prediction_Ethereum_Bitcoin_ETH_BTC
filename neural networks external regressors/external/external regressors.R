#external regressors
require(readr)
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read_csv(temp[i]))

#price = read_csv("bpi_usd.csv")
price2 = read_csv("btc_usd.csv")
price2 = price2[, 2:3]

odd_index = seq(1, 2716, 2)
price2 = price2[odd_index, ]
class(price2$timeDate)
class(price2$close)
price_df = price2 %>% 
  filter(timeDate > "2013-01-01")
price = price_df$close
price = as.numeric(price)
df = cbind(`cost-per-transaction-percent.csv`[,1], 
           `cost-per-transaction-percent.csv`[,2],
           `difficulty.csv`[,2],
           `estimated-transaction-volume-usd.csv`[,2],
           `hash-rate.csv`[,2],
           `median-confirmation-time.csv`[,2],
           `n-orphaned-blocks.csv`[,2],
           `n-transactions.csv`[,2],
           `n-unique-addresses.csv`[,2],
           `total-bitcoins.csv`[,2],
           `transaction-fees-usd.csv`[,2],
           `transaction-fees.csv`[ ,2]
           )

colnames(df) = c("date", "cost_transaction",
                 "dif", "tran_vol_usd",
                 "hr", "conf_t", "orph_blocks", 
                 "n_trans", "n_un_ad", "tot_btc", 
                 "trans_fees", "trans_fees_usd")

class(df$date)

library(dplyr)

df2 = df %>%
  filter(date > "2013-01-01")

df2 = data.frame(df2)
df2 = df2[1:898, ]
df2$price = price
df3 = df2[ ,2:ncol(df2)]
sample_index = seq(1,nrow(df3)*0.90,1) #haremos una muestra con el numero de filas que hay en parque y cogeremos el 90 % de las filas.
df3_train = df3[sample_index,] #ahora cogemos los indices creados anteriormente para utilizar dichas filas como muestra de entrenamiento
df3_test = df3[-sample_index,] #y el resto, como muestra de prueba.
library(leaps)



subset_result = regsubsets(price ~., data=df3, nbest=2, nvmax = 12)
summary(subset_result)
plot(subset_result, scale="bic") #de esta forma vemos que ha ido probando todos los modelos hasta ver cuales tienene el bic mas bajo, para seleccionar el mejor modelo posible. 


nullmodel=lm(price ~1, data=df3_train) #creamos los modelos full y null, asi que podemos empezar por cualquier de los dos. 
fullmodel=lm(price ~., data=df3_train)
#nullmodel es el modelo sin varianles solo la constante, mientras fullmodel es el modelo con todas las variables.
#vamos a probar los tres tipos, los cuales deberian dar el mismo resultado.

#Backward Elimination

model.step1 = step(fullmodel,direction='backward')
#Forward Selection

model.step2 = step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='forward')

#en ambos casos observamos que todas las variables son representativas para explicar la satisfaccion excepto weekend; es decir, el ir en fin de semana o no parece que no es representativo para explicar la satisfaccion general con la experiencia en el parque de atracciones. 

#Stepwise Selection (Output Omitted)

model.step3=step(nullmodel, scope=list(lower=nullmodel, upper=fullmodel), direction='both')

summary_model1 <- summary(model.step1)
summary_model2 <- summary(model.step2)
summary_model3 <- summary(model.step3)
summary_model1
library(forecast)
pred = forecast(object = model.step1, newdata = df3_test)
summary(pred)

pred_val = pred$mean

comprobacion = cbind(df3_test, pred_val)
require(ggplot2)

ggplot(data = comprobacion, aes(x = seq(1,90, 1))) +
  geom_line(aes(y = price)) +
  geom_line(aes(y = pred_val, color = "red"))

price3 = cbind(price_df[ ,1], price)

tempfit<-auto.arima(price3$price[1:nrow(df3_train)], xreg=df3_train[ ,1:(ncol(df3_train) -1) ])
forecast = forecast(object = tempfit, xreg = df3_test[ ,1:(ncol(df3_train) -1) ])

summary(forecast)

for_val = as.numeric(forecast$mean)

comprobacion$arima = for_val

ggplot(data = comprobacion, aes(x = seq(1,90, 1))) +
  geom_line(aes(y = price)) +
  geom_line(aes(y = for_val, color = "red"))

cor(comprobacion$price, comprobacion$arima)

df4 = df3[, c(1,2,3,4,7,8,9,10,11,12)]
df4$date = df2$date


p = c()
for (i in 600:nrow(df4)) {
  xreg = df4[1:i-1, 1:9]
  newreg = df4[i,1:9]
  arima.fit = auto.arima(df4$price[1:i-1], xreg = xreg)
  predict = forecast(object = arima.fit, xreg = newreg)
  p = c(p, predict$mean)
}

comprobacion2 = df4[600:nrow(df4), ]
comprobacion2$p = p
ggplot(data = comprobacion2, aes(x = date)) +
  geom_line(aes(y = price)) +
  geom_line(aes(y = p, color = "red")) #its clear that we have to select better the variables; these exogenous variables selected just include noise in the sample, therefore we shall take them out and look for other ones that make the work better. Probably it's not a good idea to keep on working with 2-day time series. We need more frequency to account for the variance of the price. 
#idea for tomorrow: try to change the exogenous variables included here in logarithms or differential terms; maybe in that way their magnitudes do not bias the fit and estimation. 


gold = read_csv("gold_usd.csv")
sp500 = read_csv("SP500.csv")

btc = read_csv("btc_usd.csv")
btc = btc[,2:3]
names(btc) = c("date", "btc")
require(dplyr)
btc = btc %>% 
  filter(date > "2013-01-01")


gold <- gold[seq(dim(gold)[1],1),]
gold = gold %>% 
  filter(Date > "2013-01-01" & Date < "2017-12-03")
sp500  = sp500 %>%
  filter(DATE > "2013-01-01" & DATE < "2017-12-03")
#we have a big problem here, as there are many days missing in the sp500 and gold prices. 

require(tidyr)
gold = gold %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(as.Date("2013-01-02"), as.Date("2017-12-02"), 1))

sp500 = sp500 %>%
  mutate(Date = as.Date(DATE)) %>%
  complete(Date = seq.Date(as.Date("2013-01-02"), as.Date("2017-12-02"), 1))

sp500 = sp500[, c(1,3)] #we take the good dates and the sp500 index. 

class(gold$Date)
require(zoo)
require(xts)
gold$Date = as.Date(gold$Date, format = '%d%b%Y')
#gold.xts = as.xts(gold) these dont work. 
#gold.zoo = as.zoo(gold)
#some problems that we will encounter when dealing with sp500 or gold is that these markets are not opened every day as btc, and therefore they have some absent days in nature; therefore the variability in btc in these days for which we dont have data of gold or sp500 will not be explained at all by these variables. 

#gold = gold[, 2] sp = sp500[,2]





for (i in 1:nrow(gold)) {
  if(is.na(gold$Value[i]) == FALSE) {
    next
  } else if (is.na(gold$Value[i]) == TRUE) {
    gold$Value[i] = gold$Value[i-1]
  } #we fill the NAs with the previous value; 
  #as the market is not active, the price does not change in these days. 
}

#lets do the same for sp500:
for (i in 1:nrow(sp500)) {
  if(sp500$SP500[i] == "." | is.na(sp500$SP500[i]) == TRUE) {
    sp500$SP500[i] = sp500$SP500[i-1]
  } else if (sp500$SP500[i] != "." | is.na(sp500$SP500[i]) == FALSE) {
    next
  } #we fill the NAs with the previous value; 
  #as the market is not active, the price does not change in these days. 
}

#okay now that we have cleaned the data, let us try these 2 external regressors with the btc price.

df = cbind(btc, gold)
df = cbind(df, sp500)
df = df[, c(1,2,4,6)]
df$SP500 = as.numeric(df$SP500)
#first we will try to predict one period ahead. 
p = c()
for (i in 1450:nrow(df)) {
  xreg = df[1:i-1, c(3,4)]
  newreg = df[i, c(3,4)]
  arima.fit = auto.arima(df$btc[1:i-1], xreg = xreg)
  predict = forecast(object = arima.fit, xreg = newreg)
  p = c(p, predict$mean)
}

df2 = df[1450:nrow(df), ]
df2$pred = p

ggplot(data = df2, aes(x = date)) +
  geom_line(aes(y = btc)) +
  geom_line(aes(y = pred, color = "red"))

accuracy(f = df2$pred, x = df2$btc)
cor(df2$btc, df2$pred)

#and what if we try to forecast for more periods?

arima_try = auto.arima(df$btc[1:1791], xreg = df[1:1791, c(3,4)])
pr = forecast(object = arima_try, xreg = df[1792:nrow(df), c(3,4)])
pr_mean = pr$mean

hola = df[1792:nrow(df), ]
hola$pr = pr_mean
ggplot(data = hola, aes(x = date)) +
  geom_line(aes(y = btc)) +
  geom_line(aes(y = pr, color = "red"))
#as we see, it is horrible for predicting more than 1 day... we have to keep on working on this. 
accuracy(f = hola$pr, x = hola$btc)

#we are going to work with returns for a while
lr = function(x) {
  log_returns = diff(log(x), lag=1)
}

btc_ret = lr(df$btc)
gold_ret = lr(df$Value)
sp_ret = lr(df$SP500)
dates = df$date
dates = dates[2:length(dates)]

df2 = cbind(btc_ret, gold_ret)
df2 = cbind(df2, sp_ret)

plot(x = seq(1, 1795, 1), y = btc_ret)

df2 = data.frame(df2)
pr_lr = c()
for(i in 1450:nrow(df2)) {
  xreg = df2[1:i-1, c(2,3)]
  newreg = df2[i, c(2,3)]
  arima.fit = auto.arima(df2$btc_ret[1:i-1], xreg = xreg)
  predict = forecast(object = arima.fit, xreg = newreg)
  pr_lr = c(pr_lr, predict$mean)
}

accuracy(f = pr_lr, x = df2$btc_ret[1450:nrow(df2)])

hola2 = df2[1450:nrow(df2), ]
hola2$pr = pr_lr
ggplot(data = hola2, aes(x = seq(1,346,1))) +
  geom_line(aes(y = btc_ret)) +
  geom_line(aes(y = pr, color = "red")) +
  geom_line(aes(y = gold_ret, color = "green"))





lm_ret = lm(btc_ret ~ ., data = df2)
summary(lm_ret)
