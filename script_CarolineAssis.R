library(expsmooth)
library(fma)
library(forecast)
library(tseries)
library(fpp)
library(aTSA)
library(knitr)
library(nortest)
library(ggplot2)
dados = file.choose()
dados = read.table(dados, header = T, sep = ";")
attach(dados)

summary(dados)
str(dados)

federal.ts = ts(Federal, start = 1990,end = 2019, frequency = 1)
par(mfrow=c(1,2))
acf(federal.ts)
pacf(federal.ts)
stationary.test(federal.ts, method = "pp")
# dado que não é estacionários veremos quantas diferenças serão necessárias
ndiffs(federal.ts)
# também podemos aplicar uma transformação, nesse caso vamos usar a transformação log
federal.ts_transf = ts(log(federal.ts), start = 1990, end = 2019, 
                       frequency = 1)
ndiffs(federal.ts_transf)
# vemos que mesmo com a transformação continua sendo não estácionária, então vamos partir para as diferenciações
federal.ts_diff = diff(diff(federal.ts))
stationary.test(federal.ts_diff, method = "pp")
ndiffs(federal.ts_diff)
# agora temos uma série estacionária
plot(federal.ts_diff)
acf(federal.ts_diff)
pacf(federal.ts_diff)
ndiffs(federal.ts_diff)

# estimação pelo modelo ARIMA
federal.treino = ts(federal.ts[1:25], start = 1990, frequency = 1)
federal.teste = ts(federal.ts[26:30], start = 2015, frequency = 1)
federal.aic = auto.arima(federal.treino, ic = "aic")
summary(federal.aic)
federal.bic = auto.arima(federal.treino, ic = "bic")
summary(federal.bic)
# como temos o mesmo modelo por ambos os métodos, faremos a estimação somente uma vez
federal.ajuste = arima(federal.treino, order = c(1,1,0))
summary(federal.ajuste)
# checagem do modelo
par(mfrow = c(3,1))
residuos = residuals(federal.ajuste)
plot(residuos, xlab = "Anos", ylab = "Resíduos", main = "Resíduos Padronizados")
acf(residuos, main = "ACF dos Resíduos")
qqnorm(residuos, main = "Q-Q Plot")
qqline(residuos)
Box.test(residuos, type = "Box-Pierce") #teste de correlação
lillie.test(residuos) #teste de normalidade
# previsões
previsao.2015 = forecast(federal.ajuste, lead = 1)[,"Forecast"]
previsao.2017 = forecast(federal.ajuste, lead = 3)[,"Forecast"]
previsao.2019 = forecast(federal.ajuste, lead = 5)[,"Forecast"]
previsoes = data.frame(federal.teste[c(1,3,5)], 
                       c(previsao.2019[c(1,3,5)]))
colnames(previsoes) = c("Dados Reais", "Valores Previstos")
row.names(previsoes) = c(2015, 2017, 2019)
previsoes
# obtenção das métricas
metri.2015 = accuracy(previsao.2015, federal.teste[1])
metri.2017 = accuracy(previsao.2017, federal.teste[1:3])
metri.2019 = accuracy(previsao.2019, federal.teste[1:5])
metricas = data.frame(metri.2015[2:4], metri.2017[2:4], metri.2019[2:4])
row.names(metricas) = c("RMSE", "MAE", "MPE(%)")
colnames(metricas) = c(2015, 2017, 2019)


# alisamento exponencial
AE = ses(federal.ts)$fitted
autoplot(federal.ts) +
  autolayer(AE, series="Alisamento Exponencial") +
  xlab("Anos") +
  ylab("Nº de Prisioneiros") +
  guides(colour=guide_legend(title=" "))
# previsões
previsoes.AE.2015 = ses(federal.treino, h = 1)
previsoes.AE.2017 = ses(federal.treino, h = 3)
previsoes.AE.2019 = ses(federal.treino, h = 5)
previsoes = data.frame(federal.teste[c(1,3,5)], 
                       previsoes.AE.2019$mean[c(1,3,5)])
colnames(previsoes) = c("Dados Reais", "Valores Previstos")
row.names(previsoes) = c(2015, 2017, 2019)
previsoes
# métricas
metri.AE.2015 = accuracy(previsoes.AE.2015$mean[1], federal.teste[1])
metri.AE.2017 = accuracy(previsoes.AE.2017$mean[1:3], federal.teste[1:3])
metri.AE.2019 = accuracy(previsoes.AE.2019$mean[1:5], federal.teste[1:5])
metricas.AE = data.frame(metri.AE.2015[2:4], 
                         metri.AE.2017[2:4], 
                         metri.AE.2019[2:4])
row.names(metricas.AE) = c("RMSE", "MAE", "MPE(%)")
colnames(metricas.AE) = c(2015, 2017, 2019)

# Comparação
metricas.comp = t(data.frame(metricas, metricas.AE))
row.names(metricas.comp) = c("ARIMA-2015", "ARIMA-2017", "ARIMA-2019",
                             "AE-2015", "AE-2017", "AE-2019")
metricas.comp
