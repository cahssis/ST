library(expsmooth)
library(fma)
library(forecast)
library(tseries)
library(fpp)
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
