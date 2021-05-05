library("zoo")
install.packages("forecast")
library(forecast)

##Veículos
veic <- read.table("veic.csv", sep=";", header=TRUE)
class(veic) 
veic.ts <- ts(veic, start=c(1999, 1), freq = 12)
ts.plot(veic.ts,ylab = "Veículos", xlab = "Tempo (T)" , type = "l")
veic1.hws <- HoltWinters(veic.ts , gamma = FALSE) ; veic1.hws
veic1.hws <- HoltWinters(veic.ts , alpha = 0.1,
                         beta = 0.1, gamma = FALSE) ; veic1.hws
cerro1 <- veic1.hws$SSE
veic1_fit<-veic1.hws$fitted
veic1x_fit <- veic1_fit[,1]
veic1l_fit <- veic1_fit[,2]
veic1t_fit <- veic1_fit[,3]

veic2.hws <- HoltWinters(veic.ts, alpha = 0.3,
                         beta = 0.2 , gamma = FALSE) ; veic2.hws
cerro2 <- veic2.hws$SSE
veic2_fit <- veic2.hws$fitted
veic2x_fit <- veic2_fit[,1]
veic2l_fit <- veic2_fit[,2]
veic2t_fit <- veic2_fit[,3]

layout(1:1)
ts.plot(cbind(veic.ts,veic1x_fit),
        ylab = "Veículos ---  S1(A=0,1 e B=0,1) - - ",
        xlab = "Tempo (T)" , lty= c(1:3))
ts.plot(cbind(veic.ts,veic2x_fit), 
        ylab = "Veículos ---  S1(A=0,3 e B=0,2) - - ",
        xlab = "Tempo (T)" , lty= c(1:3))
layout(1:1)
#estima antes sem valores
veicr.hws <- HoltWinters(veic.ts, gamma = FALSE,) ; veicr.hws
#observa os resultados e faz a estimativa
veice.hws <- HoltWinters(veic.ts, alpha = 0.37, beta = 0.12,
                         gamma = FALSE) ; veice.hws
cerroe <- veice.hws$SSE
veice_fit <- veic2.hws$fitted
veicex_fit <- veic2_fit[,1]
veicel_fit <- veic2_fit[,2]
veicet_fit <- veic2_fit[,3]
ts.plot(cbind(veic.ts, veicex_fit),
        ylab = "Veículo --    A=0,37 e B=0,12 - - ",
        xlab = "Tempo (T)",  lty = c(1:2))
#SQE
cerro1 #Erro A=0,1 e B=0,1
cerro2 #Erro A=0,3 e B=0,2
cerroe #Erro A=0,37 e B=0,12
#Graficos da previsão
prev1 <- forecast(veic2.hws, h=12, level=95)
prev3 <- forecast(veice.hws, h=12, level=95)
layout(1:1)
plot(prev1, ylab = "Veículo", xlab = "Tempo", main = '')
plot(prev3, ylab = "Veículo", xlab = "Tempo", main = '')
layout(1:1)


#Holt aditiva e multiplicativa, melhores coeficientes
veics_a.hws <- HoltWinters(veic.ts, seasonal = "addit", ) ; veics_a.hws
veics_m.hws <- HoltWinters(veic.ts, seasonal = "mult") ; veics_m.hws

#Estimativas 
veic_a.hws <- HoltWinters(veic.ts, seasonal = "addit",
                          alpha = 0.22, beta = 0.06 , gamma = 0.21,
                          b.start=0) ; veic_a.hws
veic_m.hws <- HoltWinters(veic.ts, seasonal = "mult",
                          alpha = 0.23, beta = 0.06 , gamma = 0.23,
                          b.start=0) ; veic_m.hws

veica_fit <-veic_a.hws$fitted
veicax_fit <- veica_fit[,1]
veical_fit <- veica_fit[,2]
veicat_fit <- veica_fit[,3]
veicas_fit <- veica_fit[,3]

veicm_fit <-veic_a.hws$fitted
veicmx_fit <- veicm_fit[,1]
veicml_fit <- veicm_fit[,2]
veicmt_fit <- veicm_fit[,3]
veicms_fit <- veicm_fit[,3]

layout(1:1)
ts.plot(cbind(veic.ts, veicax_fit), 
        ylab = "Veículo --    A=0.22 e B=0.06 e C=0.21 - - ",
        xlab = "Tempo (Aditivo)", lty=c(1,3))
ts.plot(cbind(veic.ts, veicmx_fit), 
        ylab = "Veículo  --    A=0.23 e B=0.06 e C=0.23 - - ",
        xlab = "Tempo (Multiplicativo)", lty=c(1,3))

#Graficos da previsão
prevA <- forecast(veic_a.hws, h=12, level=95)
prevM <- forecast(veic_m.hws, h=12, level=95)

#Erros
veic_a.hws$SSE	#Modelo Aditivo
veic_m.hws$SSE	#Modelo Multiplicativo

layout(1:1)
plot(prevA, ylab = "Veículo", xlab = "Tempo", main = '')
plot(prevM, ylab = "Veículo", xlab = "Tempo", main = '')



#Nova forma do gráfico com cores
plot(veic.ts, ylab = "Veículo", xlab = "Tempo", main = '')
lines(veicex_fit,lwd=2 , col='red')
legend(1999, 25000,c('Veículo', 'Ajuste'), lwd=c(1,2), col=c('black','red'), bty='n')



