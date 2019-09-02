---
title: "Queimadas - Brasil e Amazônia"
author: "Cassiano Ricardo Dalberto"
date: "2 de setembro de 2019"
output:
  html_document:
    keep_md: yes
---



### Carregar bibliotecas necessárias

```r
library(forecast)
library(ggplot2)
library(lubridate)
library(strucchange)
library(ggfortify)
```

# Parte 1: Análise para o Brasil

Definir diretório de trabalho e carregar dados:

```r
data <- read.csv("queimadas br.csv")
data_ts <- ts(data$Queimadas, start=c(1998, 6), end=c(2019, 8), frequency = 12) # Definir série de tempo
```

### Decompondo a série de tempo

Nota:  os gráficos como estão só funcionam com o forecast versão 8.8. Se possuir uma versão mais recente, é preciso desinstalar e instalar a 8.8 com os comandos:

```r
packageurl <- "https://cran.r-project.org/src/contrib/Archive/forecast/forecast_8.8.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
```

Decomposição e gráfico:

```r
decomp <- stl(data_ts, "periodic") # Decomposição
autoplot(decomp) + scale_x_continuous(breaks = seq(1998, 2019, by=1)) + 
    theme(axis.text.x = element_text(angle = 90))+
    geom_rect(aes(xmin=2019, xmax=2019.8, ymin=0, ymax=Inf), alpha=.002, fill="red")+
    theme(axis.title.x = element_blank())+
    ggtitle("Decomposição da série de queimadas no Brasil - jun/1998 a ago/2019")
```

Obter a série sem sazonalidade e adicionar respectivas datas:

```r
dess_data <- as.data.frame(decomp$time.series)
values = as.data.frame(seq(from = as.Date("1998-06-01"), to = as.Date("2019-08-01"), by = 'month'))
dess_data$time <- values
colnames(dess_data[,4]) <-"time"
```

### Gráfico da série dessazonalizada:

```r
queim_plot <- ggplot(dess_data) + geom_line(aes(y=trend, x=time)) + 
    scale_x_date(breaks = "1 year",date_labels = "%Y")+ 
    theme(axis.text.x = element_text(angle = 90))+
    geom_rect(aes(xmin=as.Date("2019-01-01", "%Y-%m-%d"), xmax=as.Date("2019-08-01", "%Y-%m-%d"), 
                  ymin=0, ymax=Inf), alpha=.002, fill="red")+
    theme(axis.title.x = element_blank()) + labs(y="Queimadas (ciclo + tendência)")+
    ggtitle("Queimadas no Brasil - jun/1998 a ago/2019")
```

Teste simples de relação com o El Niño:

```r
nino <- read.csv("el nino.csv")
reg_queim <- lm(dess_data$trend[1:253] ~ nino$El_nino)
summary(reg_queim)
```

### Testando para existência de quebras estruturais na série:

```r
trend_ts <- ts(dess_data$trend,  start=c(1998, 6), end=c(2019, 8), frequency = 12) # Definir série de tempo

trend_breaks <- breakpoints(trend_ts ~ 1) # Obter as quebras e seus intervalos de confiança
summary(trend_breaks)
trend_breaks_ci <- confint(trend_breaks)
```

Transformar os dados dos intervalos em confiança em datas para usar no ggplot:

```r
df_ci <- as.data.frame(trend_breaks_ci$confint)

datas_intervalos <- matrix(nrow=nrow(df_ci), ncol=ncol(df_ci))
datass <- for(i in 1:nrow(df_ci)){
    for(j in 1:ncol(df_ci)){
        datas_intervalos[i,j] <- as.Date(as.yearmon("1998-05") + df_ci[i,j]/12)
    }       
}

colnames(datas_intervalos)[1:3] <- c("lower", "break", "upper")
datas_intervalos <- as.data.frame(datas_intervalos)
datas_intervalos[,1:3] = lapply(datas_intervalos[,1:3], as.Date)
```

### Gráfico da série dessazonalizada com as quebras estruturais e seus respectivos intervalos de confiança:

```r
graf_base <- autoplot(breakpoints(trend_ts ~ 1), ts.colour = 'black', ts.linetype = 'solid', ts.size=1,
              cpt.colour = 'dodgerblue3', cpt.linetype = 'solid')

quebras <- graf_base + scale_x_date(breaks = "1 year",date_labels = "%Y")+ 
    theme(axis.text.x = element_text(angle = 90))+
    geom_rect(aes(xmin=as.Date("2019-01-01", "%Y-%m-%d"), xmax=as.Date("2019-08-01", "%Y-%m-%d"), 
                  ymin=0, ymax=Inf), alpha=.002, fill="red")+
    theme(axis.title.x = element_blank()) + labs(y="Queimadas (ciclo + tendência)")+
    geom_errorbarh(data=datas_intervalos, aes(y=0, x=upper, xmin=upper, xmax=lower), height=1, size=1, color="red")+
    ggtitle("Queimadas no Brasil - jun/1998 a ago/2019")
```


# Parte 2: Análise para a Amazônia

Carregar dados:

```r
amazonia <- read.csv("Amazonia.csv")
amazonia_ts <- ts(amazonia$Queimadas, start=c(1998, 6), end=c(2019, 8), frequency = 12) # Definir série de tempo
```

Decomposição e gráfico:

```r
decomp_am <- stl(amazonia_ts, "periodic") # Decomposição
autoplot(decomp_am) + scale_x_continuous(breaks = seq(1998, 2019, by=1)) + 
    theme(axis.text.x = element_text(angle = 90))+
    geom_rect(aes(xmin=2019, xmax=2019.8, ymin=0, ymax=Inf), alpha=.002, fill="red")+
    theme(axis.title.x = element_blank())+
    ggtitle("Decomposição da série de queimadas na Amazônia - jun/1998 a ago/2019")
```

Obter a série sem sazonalidade e adicionar respectivas datas:

```r
dess_data_amazonia <- as.data.frame(decomp_am$time.series)
values = as.data.frame(seq(from = as.Date("1998-06-01"), to = as.Date("2019-08-01"), by = 'month'))
dess_data_amazonia$time <- values
colnames(dess_data_amazonia[,4]) <-"time"
```

### Gráfico da série dessazonalizada:

```r
queim_plot_am <- ggplot(dess_data_amazonia) + geom_line(aes(y=trend, x=time)) + 
    scale_x_date(breaks = "1 year",date_labels = "%Y")+ 
    theme(axis.text.x = element_text(angle = 90))+
    geom_rect(aes(xmin=as.Date("2019-01-01", "%Y-%m-%d"), xmax=as.Date("2019-08-01", "%Y-%m-%d"), 
                  ymin=0, ymax=Inf), alpha=.002, fill="red")+
    theme(axis.title.x = element_blank()) + labs(y="Queimadas (ciclo + tendência)")+
    ggtitle("Queimadas na Amazônia - jun/1998 a ago/2019")
```

### Testando para existência de quebras estruturais na série:

```r
amazon_trend_ts <- ts(dess_data_amazonia$trend,  start=c(1998, 6), end=c(2019, 8), frequency = 12) # Definir série de tempo

amazon_trend_breaks <- breakpoints(amazon_trend_ts ~ 1) # Obter as quebras e seus intervalos de confiança
summary(amazon_trend_breaks)
amazon_trend_breaks_ci <- confint(amazon_trend_breaks)
```

Transformar os dados dos intervalos em confiança em datas para usar no ggplot:

```r
df_ci_am <- as.data.frame(amazon_trend_breaks_ci$confint)

datas_interv_am <- matrix(nrow=nrow(df_ci_am), ncol=ncol(df_ci_am))
datass <- for(i in 1:nrow(df_ci_am)){
    for(j in 1:ncol(df_ci_am)){
        datas_interv_am[i,j] <- as.Date(as.yearmon("1998-05") + df_ci_am[i,j]/12)
    }       
}
colnames(datas_interv_am)[1:3] <- c("lower", "break", "upper")
datas_interv_am <- as.data.frame(datas_interv_am)
datas_interv_am[,1:3] = lapply(datas_interv_am[,1:3], as.Date)
```

### Gráfico da série dessazonalizada com as quebras estruturais e seus respectivos intervalos de confiança:

```r
graf_base_am <- autoplot(breakpoints(amazon_trend_ts ~ 1), ts.colour = 'black', ts.linetype = 'solid', ts.size=1,
         cpt.colour = 'dodgerblue3', cpt.linetype = 'solid')

quebras_am <- graf_base_am + scale_x_date(breaks = "1 year",date_labels = "%Y")+ 
    theme(axis.text.x = element_text(angle = 90))+
    geom_rect(aes(xmin=as.Date("2019-01-01", "%Y-%m-%d"), xmax=as.Date("2019-08-01", "%Y-%m-%d"), 
                  ymin=0, ymax=Inf), alpha=.002, fill="red")+
    theme(axis.title.x = element_blank()) + labs(y="Queimadas (ciclo + tendência)")+
    geom_errorbarh(data=datas_interv_am, aes(y=0, x=upper, xmin=upper, xmax=lower), height=1, size=1, color="red")+
    ggtitle("Queimadas na Amazônia - jun/1998 a ago/2019")
```
