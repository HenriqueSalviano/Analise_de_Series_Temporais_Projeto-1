#### Analise descritiva
library(TSA)
summary(UKDriverDeaths)
hist(UKDriverDeaths, col = "lavenderblush3", main = "Histograma das vítimas de rodovias na Grã-Bretanha 1969-84", xlab = "totais mensais", ylab = "Frequ^encia", xlim = c(1000,3000))
boxplot(UKDriverDeaths,col = "lavenderblush3", main = "Boxplot das vítimas de rodovias na Grã-Bretanha 1969-84", ylab = "totais mensais")
######################## Gr´afico temporal
ts.plot(UKDriverDeaths,main = "Gráfico Temporal",
        gpars=list(xlab= "Tempo", ylab="totais mensais"))
lines(filter(UKDriverDeaths, c(1/2, rep(1, 11), 1/2)/12), col = 2)


#### Sazonalidade
library(forecast)
library(ggplot2)

ggseasonplot(UKDriverDeaths) +
  ggtitle("Sazonalidade") +
  labs(x = "") +
  scale_colour_discrete(name = "Ano") +
  theme_bw()
monthplot(UKDriverDeaths,
          main = "Gráfico de Série Temporal Separado por Mês",
          ylab = "Mortes e Acidentes",
          xlab = "Meses")


########### Médias Moveis
# Media movel
med.moveis<-function(x,k){
  x<-as.vector(x)
  N<-length(x)
  xstar<-rep(0,N)
  for (i in 1:k) xstar[i]<-NA
  for (i in (1+k):(N-k)) xstar[i]<-mean(x[(i-k):(i+k)],na.rm=TRUE)
  for (i in (N-k+1):N) xstar[i]<-NA
  ts.plot(x, main=paste0("Média Móvel para k = ",k),
          gpars=list(xlab="Tempo"))
  lines(xstar, col=2)
  xstar
}
med.moveis(UKDriverDeaths,12)


################ Verificando a tend^encia
############################ Testes de Tend^encia
#### Teste de Wald-Wolfowitz
library(randtests)
library(trend)
runs.test(UKDriverDeaths)
########### Teste Cox Stuart
cox.stuart.test(UKDriverDeaths)
# Grafico de amplitude media
med.var<-function(x,k){
  N<-length(x)
  x.m<-rep(0,(N-k))
  x.r<-rep(0,(N-k))
  for (i in 1:(N-k)) x.m[i]<-mean(x[i:(i+k)])
  for (i in 1:(N-k)) x.r[i]<-max(x[i:(i+k)])-min(x[i:(i+k)])
  plot(x.m,x.r,xlab="medias",ylab="amplitude")
  aa1<-lm(x.r~x.m)
  abline(aa1$coef[1],aa1$coef[2],col=2)
  summary(aa1)
}
med.var((UKDriverDeaths),12)


######## Tirando a tendencia, sazonalidade e homocedasticidade
ts.plot(diff(diff(log(UKDriverDeaths))), main = "Gráfico temporal sem tendência e sem sazonalidade",
        gpars = list(xlab= "Tempo (anos)", ylab = ""))

##### Decompose
set.seed(1234)
x <- filter(rnorm(100), 0.9, method = "recursive")
dd_dec <- decompose(log(UKDriverDeaths))
dd_stl <- stl(log(UKDriverDeaths), s.window = 13)
plot(dd_dec)


######### Verificando auto correlação
acf(diff(diff((log(UKDriverDeaths)))))
########################### auto correlação parcial
pacf(diff(diff(log(UKDriverDeaths))))
m1_novo<-arima(UKDriverDeaths,order = c(1,0,1))
m1_novo
tsdiag(m1_novo)


############################### Interverção de 31/01/1983 uso do cinto
### de segurança.
# Ajustando Modelo SARIMA (1,0,1)(0,1,1) sem a intervenção

m1 = arimax(log(UKDriverDeaths),order=c(1,0,1),seasonal=list(order=c(0,1,1),period=12))
m1;tsdiag(m1)

# ACF da intervenção
acf(as.vector(diff(diff(window(log(UKDriverDeaths),end=c(1983,8)),12))),lag.max=48)

# Ajustando Modelo SARIMA (1,0,1)(0,1,1) com a intervenção
m1_inter=arimax(log(UKDriverDeaths),order=c(1,0,1),seasonal=list(order=c(0,1,1),
                                                                 period=12),
                xtransf=data.frame(I3101=1*(seq(UKDriverDeaths)== 170)),
                transfer=list(c(0,0)),method='ML')
m1_inter;tsdiag(m1_inter)

# Plotando os pontos estimados por nosso modelo
plot(log(UKDriverDeaths),ylab="log(UKDriverDeaths)")
points(fitted(m1_inter),col=2)

