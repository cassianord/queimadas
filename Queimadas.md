---
title: "Queimadas - Brasil e Amazônia"
author: "Cassiano Ricardo Dalberto"
date: "1 de setembro de 2019"
output:
  html_document: 
    keep_md: true
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
