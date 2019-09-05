# read .csv containing monthly inflation values from IPConline
setwd('C:\\Users\\Flor\\OneDrive\\DataInflacion')
IPC<-read.csv("serie-historica-ipconline-4.csv", header = TRUE, sep=";")

colnames(IPC)[1]<-"Fecha"
colnames(IPC)[2]<-"General"
colnames(IPC)[5]<-"Vivienda"
colnames(IPC)[7]<-"Salud"

#install.packages("xts")
library(xts)

#generate index
aux<-as.POSIXct(strptime(as.character(IPC$Fecha), "%Y-%m-%d"))
index<-as.Date(aux)
#index<-as.Date(aux, format="%Y-%m-%d")

#index<-seq(as.Date("2014-09-01"), length = 59, by = "days")

#IPC_xts <- xts(x = IPC[,-1], orderby = index)
IPC_xts <- xts(x = IPC$General, orderby = index)

acf(IPC$General) #autocorrelation
pacf(IPC$General) #partial autocorrelation

#PACF tails at lag 1, ACF tails off ==> AR model order 1

#IPC_xts<-xts(x=IPC[,-1],orderby=index)

#fit ARIMA model
AR <- arima(IPC$General,order=c(1,0,0))

ts.plot(IPC$General,xlim = c(0, 80), ylim = c(-2,6))
AR_fitted <- IPC$General - residuals(AR)
points(AR_fitted, type = "l", col = 2, lty = 2)

AR_forecast <- predict(AR, n.ahead = 20)$pred
AR_forecast_se <- predict(AR, n.ahead = 20)$se
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)

# package astsa
#install.packages("astsa")
library(astsa)

AR1 <- sarima(IPC$General,p=1,d=0,q=0)
AR1$ttable

AR1MA1 <- sarima(IPC$General,p=1,d=0,q=1)
AR1MA1$ttable

AR1$AIC
AR1$BIC

AR1MA1$AIC
AR1MA1$BIC

#almost the same AIC and BIC, choose simpler model
#forecast next 10 months' inflation
sarima.for(IPC$General,n.ahead = 10,1,0,0)

