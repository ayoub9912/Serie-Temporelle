Data=read.csv('C:/Users/Ayoub/OneDrive/Bureau/3eme LAID/ST/USD_SGD.csv')
serie =ts(Data, start=c(1995, 12), end=c(2020, 12), frequency=12)
serie
library(tseries)
#representation graphique
plot.ts(serie[,2])
#Q2
#I)Test de dickey-fuller pour verifier la presence de stationnarite

View(serie[,6])
adf.test(serie[,6])
#Pvalue<0.05==>La serie est stationnaire

#test de la presence d'autocorrelation

Box.test(serie[,6])
#p_value>0.05===>absence d'autocorrelation
pacf(serie[,6])
#estimation d'un modèle AR
ar(serie[,6])
#Modele AR(3)=X(t)=0.049x(t-1)+0.162x(t-2)+0.093x(t-3)+ep(t)
#variance ep(t)=3078

#estimation d'un modèle MA
acf(serie[,6])
msft_ma =arma(x = serie[,6], order = c(0,3))
msft_ma
residuals <- residuals(msft_ma)
msft_fitted <- serie[,6] - residuals
#VisualisationMA
ts.plot(serie[,6])
points(msft_fitted, type = "p", col = 4, lty = 2)
#VisualisationAR
ar=arima.sim(list(ar=c(0.049,0.162,0.093)),n=301)
plot.ts(serie[,6])
plot.ts(ar,col="red")
#VisualisationARMA
msft_arma =arma(x = serie[,6], order = c(3,3))
msft_arma
residuals <- residuals(msft_arma)
msft_fitted <- serie[,6] - residuals
ts.plot(serie[,6])
points(msft_fitted, type = "p", col = 6, lty = 2)
#Onchoisit le modèle ARMA


