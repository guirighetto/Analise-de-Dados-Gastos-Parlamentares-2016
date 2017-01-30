#library
library(plyr)
library(plotly)
library(reshape2)
source("/home/guilherme/Documentos/GastosBrasil/lib.R")

#load data csv
despesas = read.csv("/home/guilherme/Documentos/GastosBrasil/Despesas.csv")
partidos = read.csv("/home/guilherme/Documentos/GastosBrasil/Partidos.csv")

#Analyse
#Gasto de cada partido
despesas.total.partidos <- aggregate(vlrLiquido ~ sgPartido, despesas, sum, na.rm=TRUE)
despesas.mes.partidos <- aggregate(despesas$vlrLiquido,list(sgPartido = despesas$sgPartido, mes = despesas$numMes), sum)
despesas.total.partidos <- despesas.total.partidos[-1,]
despesas.politicos.requisicoes <- count(despesas,c("txNomeParlamentar","sgPartido"))
despesas.politicos.partidos <- count(despesas.politicos.requisicoes$sgPartido)
despesas.politicos.partidos.pie <- despesas.politicos.partidos[-1,]
despesas.politicos.partidos.pie$x <- paste(despesas.politicos.partidos[-1,]$x,despesas.politicos.partidos[-1,]$freq)

despesas.media.partidos.anual <- data.frame(despesas.total.partidos$sgPartido, despesas.total.partidos$vlrLiquido/despesas.politicos.partidos[-1,]$freq)
despesas.media.partidos.mensal <- data.frame(despesas.total.partidos$sgPartido, (despesas.total.partidos$vlrLiquido/despesas.politicos.partidos[-1,]$freq)/12)

despesas.ressarcido <- subset(despesas, despesas$numRessarcimento != 0)
despesas.naoressarcido <- subset(despesas, despesas$numRessarcimento == 0)

despesas.total.ressarcido <- aggregate(vlrLiquido ~ sgPartido, despesas.ressarcido, sum, na.rm=TRUE) 
despesas.total.ressarcido <- despesas.total.ressarcido[-1,]
despesas.total.naoressarcido <- aggregate(vlrLiquido ~ sgPartido, despesas.naoressarcido, sum, na.rm=TRUE)
despesas.total <- data.frame(despesas.total.naoressarcido$sgPartido, despesas.total.naoressarcido$vlrLiquido, despesas.total.ressarcido$vlrLiquido)

despesas.gastos.total <- data.frame(despesas.total.partidos,despesas.politicos.partidos[-1,]$freq)

#Mes
names(despesas.mes.partidos)[names(despesas.mes.partidos)=="x"] <- "vlrLiquido"
despesas.mes <- NULL
despesas.mes$sgPartido <- despesas.total.partidos$sgPartido
despesas.mes <- data.frame(despesas.mes)
meses <- c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")

i<-1
j<-2
k<-26
while(i < length(meses)+1)
{
  despesas.mes[[meses[i]]] = 0
  despesas.mes <- addInequalColumn(despesas.total.partidos,despesas.mes.partidos[j:k,],despesas.mes,meses[i])
  j<-k+2
  if(i < 6)
    k<-k+26
  else if(i < 11)
    k<-k+28
  else
    k<-k+27
  i<-i+1
}

#Plot 1 Gasto Mes
plot_ly(despesas.mes, y = ~despesas.mes$fev, x = ~despesas.mes$sgPartido, type = 'scatter', name = 'Fev', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$mar, name = 'Mar', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$abr, name = 'Abr', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$mai, name = 'Mai', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$jun, name = 'Jun', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$jul, name = 'Jul', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$ago, name = 'Ago', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$set, name = 'Set', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$out, name = 'Out', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$nov, name = 'Nov', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$dez, name = 'Dez', mode = 'lines+markers') %>%
add_trace(y = ~despesas.mes$jan, name = 'Jan', mode = 'lines+markers') %>%
layout(yaxis = list(title = 'Valor (R$)'), xaxis = list(title = ''),title = "Gastos de cada mês - Partidos - 2016~2017")

#Plot 2 Gasto Mes
plot_ly(despesas.mes, y = ~despesas.mes$fev, x = ~despesas.mes$sgPartido, type = 'bar', name = 'Fev') %>%
add_trace(y = ~despesas.mes$mar, name = 'Mar') %>%
add_trace(y = ~despesas.mes$abr, name = 'Abr') %>%
add_trace(y = ~despesas.mes$mai, name = 'Mai') %>%
add_trace(y = ~despesas.mes$jun, name = 'Jun') %>%
add_trace(y = ~despesas.mes$jul, name = 'Jul') %>%
add_trace(y = ~despesas.mes$ago, name = 'Ago') %>%
add_trace(y = ~despesas.mes$set, name = 'Set') %>%
add_trace(y = ~despesas.mes$out, name = 'Out') %>%
add_trace(y = ~despesas.mes$nov, name = 'Nov') %>%
add_trace(y = ~despesas.mes$dez, name = 'Dez') %>%
add_trace(y = ~despesas.mes$jan, name = 'Jan') %>%
layout(yaxis = list(title = 'Valor (R$)'), xaxis = list(title = ''),title = "Gastos de cada mês - Partidos - 2016~2017", barmode = 'group')

#Plot Total Parlamentares
plot_ly(despesas.politicos.partidos.pie, labels = ~x, values = ~freq, type = 'pie') %>%
layout(title = 'Quantidade de Parlamentares - Partidos',
       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Plot Gastos/TotalParlamentares
plot_ly(despesas.gastos.total, x = ~despesas.gastos.total$sgPartido, y = ~despesas.gastos.total$vlrLiquido, size = ~despesas.gastos.total$despesas.politicos.partidos..1....freq, type = 'scatter',mode = 'markers') %>%
layout(yaxis = list(title = 'Valor (R$)'), xaxis = list(title = ''),title = "Total Gastos - Número de Parlamentares (Tamanho do Marcador)")

#Plot Media dos Gastos - Anual
plot_ly(despesas.media.partidos.anual,y = ~despesas.media.partidos.anual$despesas.total.partidos.sgPartido,x=~despesas.media.partidos.anual$despesas.total.partidos.vlrLiquido.despesas.politicos.partidos..1..,type = 'bar')  %>%
layout(xaxis = list(title = 'Valor (R$)'), yaxis = list(title = ''),title = "Média de gastos - Parlamentar - Anual")

#Plot Media dos Gastos - Mensal
plot_ly(despesas.media.partidos.mensal,y = ~despesas.media.partidos.mensal$despesas.total.partidos.sgPartido,x=~despesas.media.partidos.mensal$X.despesas.total.partidos.vlrLiquido.despesas.politicos.partidos..1..,type = 'bar')  %>%
layout(xaxis = list(title = 'Valor (R$)'), yaxis = list(title = ''),title = "Média de gastos - Parlamentar - Mensal")

#Plot Total dos Gastos
plot_ly(despesas.total, y = ~despesas.total$despesas.total.naoressarcido.vlrLiquido, x = ~despesas.total$despesas.total.naoressarcido.sgPartido, type = 'bar', name = 'Não Ressarcido') %>%
add_trace(y = ~despesas.total$despesas.total.ressarcido.vlrLiquido, name = 'Ressarcido') %>%
layout(yaxis = list(title = 'Valor (R$)'), xaxis = list(title = ''),title = "Total de gastos - Partidos - 2016", barmode = 'stack')

#Analyse 2
despesas.parlamentar.sudeste <- subset(despesas, despesas$sgUF == 'SP' | despesas$sgUF == 'RJ' | despesas$sgUF == 'ES' | despesas$sgUF == 'MG')
despesas.parlamentar.sul <- subset(despesas, despesas$sgUF == 'PR' | despesas$sgUF == 'SC' | despesas$sgUF == 'RS')
despesas.parlamentar.centro <- subset(despesas, despesas$sgUF == 'MT' | despesas$sgUF == 'GO' | despesas$sgUF == 'MS')
despesas.parlamentar.nordeste <- subset(despesas, despesas$sgUF == 'BA' | despesas$sgUF == 'MA' | despesas$sgUF == 'PI' | despesas$sgUF == 'CE' | despesas$sgUF == 'RN' | despesas$sgUF == 'PB' | despesas$sgUF == 'PE' | despesas$sgUF == 'AL' | despesas$sgUF == 'SE')
despesas.parlamentar.norte <- subset(despesas, despesas$sgUF == 'RO' | despesas$sgUF == 'AC' | despesas$sgUF == 'AM' | despesas$sgUF == 'PA' | despesas$sgUF == 'TO' | despesas$sgUF == 'AP' | despesas$sgUF == 'RR')

despesas.total.sudeste <- aggregate(vlrLiquido ~ sgPartido, despesas.parlamentar.sudeste, sum, na.rm=TRUE) 
despesas.total.sul <- aggregate(vlrLiquido ~ sgPartido, despesas.parlamentar.sul, sum, na.rm=TRUE) 
despesas.total.centro <- aggregate(vlrLiquido ~ sgPartido, despesas.parlamentar.centro, sum, na.rm=TRUE) 
despesas.total.nordeste <- aggregate(vlrLiquido ~ sgPartido, despesas.parlamentar.nordeste, sum, na.rm=TRUE) 
despesas.total.norte <- aggregate(vlrLiquido ~ sgPartido, despesas.parlamentar.norte, sum, na.rm=TRUE)

despesas.regioes <- NULL
despesas.regioes$sgPartido <- despesas.total.partidos$sgPartido
despesas.regioes$sudeste = 0
despesas.regioes$sul = 0
despesas.regioes$centro = 0
despesas.regioes$nordeste = 0
despesas.regioes$norte = 0
despesas.regioes <- data.frame(despesas.regioes)

despesas.regioes <- addInequalColumn(despesas.total.partidos,despesas.total.sudeste,despesas.regioes,"sudeste")
despesas.regioes <- addInequalColumn(despesas.total.partidos,despesas.total.sul,despesas.regioes,"sul")
despesas.regioes <- addInequalColumn(despesas.total.partidos,despesas.total.centro,despesas.regioes,"centro")
despesas.regioes <- addInequalColumn(despesas.total.partidos,despesas.total.nordeste,despesas.regioes,"nordeste")
despesas.regioes <- addInequalColumn(despesas.total.partidos,despesas.total.norte,despesas.regioes,"norte")

#Plot Gasto Anual por Regiao
plot_ly(despesas.regioes, y = ~sudeste, x = ~sgPartido, type = 'bar', name = 'Sudeste') %>%
add_trace(y = ~sul, name = 'Sul') %>%
add_trace(y = ~centro, name = 'Centro') %>%
add_trace(y = ~nordeste, name = 'Nordeste') %>%
add_trace(y = ~norte, name = 'Norte') %>%
layout(yaxis = list(title = 'Valor (R$)'), xaxis = list(title = ''),title = "Total de gastos anual - Região - 2016", barmode = 'group')

#Partidos
partidos.espc <- NULL
partidos.espc$sgPartido <- despesas.gastos.total$sgPartido
partidos.espc$vlrLiquido <- despesas.gastos.total$vlrLiquido
partidos.espc <- data.frame(partidos.espc)
 
list.partidos <- despesas.gastos.total$sgPartido
i<-1
while(i < length(despesas.total.partidos$sgPartido)+1)
{
   tmp<-NULL
   tmp <- partidos[partidos$Sigla == as.character(list.partidos[i]), ]
   partidos.espc[i,3] = as.character(tmp$Espectro)
   i<-i+1
}

names(partidos.espc)[names(partidos.espc)=="V3"] <- "Espectro"

partidos.espc.gastos <- aggregate(vlrLiquido ~ Espectro, partidos.espc, sum, na.rm=TRUE) 

#Plot Gastos por Espectro
plot_ly(partidos.espc.gastos,x="",y=~partidos.espc.gastos$vlrLiquido[1],type = 'bar',name="Centro")  %>%
add_trace(y=~partidos.espc.gastos$vlrLiquido[2], name ="Centro a Centro-direita ") %>%
add_trace(y=~partidos.espc.gastos$vlrLiquido[3], name ="Centro-direita") %>%
add_trace(y=~partidos.espc.gastos$vlrLiquido[4], name ="Centro-direita e Direita") %>%
add_trace(y=~partidos.espc.gastos$vlrLiquido[5], name ="Centro e Centro-esquerda") %>%
add_trace(y=~partidos.espc.gastos$vlrLiquido[6], name ="Centro-esquerda") %>%
add_trace(y=~partidos.espc.gastos$vlrLiquido[7], name ="Direita") %>%
add_trace(y=~partidos.espc.gastos$vlrLiquido[8], name ="Esquerda a Centro-esquerda") %>%
add_trace(y=~partidos.espc.gastos$vlrLiquido[9], name ="Esquerda a Extrema-esquerda") %>%
layout(yaxis = list(title = 'Valor (R$)'), xaxis = list(title ="Espectros"),title = "Total de Gastos Anual - Espectros - Partidos")

