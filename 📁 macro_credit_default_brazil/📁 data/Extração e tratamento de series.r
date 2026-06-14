# 1. Instalar e carregar pacotes necessários

install.packages("ggplot2")
library(GetBCBData)
library(tidyverse)
library(lubridate)
library(vars)
library(urca)
library(lmtest)
library(quantmod)
library(jsonlite)
library(forecast)
library(FinTS)
library(rugarch)
library(TSstudio)
library(ggplot2)


setwd("C:/Temp/AST")
igp <-read.csv2("igp-m-fgv.csv", header = TRUE)
igp <- na.omit(as.numeric(igp$Índice))
plot(igp)
PIB <- read.csv2("C:/Temp/AST/pib br fed (2).csv", header = TRUE)
PIB <- na.omit(as.numeric(PIB$valor))
plot(PIB)

# 2. Coletar dados do Banco Central

#"Sys.Date()

# Função auxiliar para processar média trimestral
processar_trimestre <- function(id, nome, data_inicio) {
  df <- gbcbd_get_series(id, first.date = data_inicio, last.date = "2025-09-30")
  
  df %>%
    rename(data = ref.date, valor = value) %>%
    mutate(
      ano = year(data),
      trimestre = quarter(data),
      serie = nome
    ) %>%
    group_by(serie, ano, trimestre) %>%
    summarise(media = mean(valor, na.rm = TRUE), .groups = 'drop') %>%
    mutate(periodo = paste0(ano, " Q", trimestre))
}

# 2. Chamar Séries Separadamente (Desde 2011)
data_ref <- "2011-01-01"


# Inadimplência
inad_pf <- processar_trimestre(21084, "Inadimplência PF", data_ref)
inad_pf <- na.omit(as.numeric(inad_pf$media))
inad_pj <- processar_trimestre(21083, "Inadimplência PJ", data_ref)
inad_pj <- na.omit(as.numeric(inad_pj$media))
# Taxas de Juros
juros_pf <- processar_trimestre(25470, "Juros PF (% a.m.)", data_ref)
juros_pf <- na.omit(as.numeric(juros_pf$media))
juros_pj <- processar_trimestre(25444, "Juros PJ (% a.m.)", data_ref)
juros_pj <- na.omit(as.numeric(juros_pj$media))
# Índice de Basileia
basileia <- processar_trimestre(29661, "Juros PJ (% a.m.)", data_ref)
basileia <- na.omit(as.numeric(basileia$media))
plot(inad_pf)
plot(inad_pj)
plot(juros_pf)
plot(juros_pj)
plot(basileia)
plot(PIB)
plot(igp)


# Configura a grade (4 linhas, 2 colunas)
par(mfrow = c(4, 2))

# Executa os plots em sequência
plot(inad_pf)
plot(inad_pj$periodo, inad_pj$media, main="Inadimplência PJ", type="l")
plot(juros_pf$periodo, juros_pf$media, main="Juros PF", type="l")
plot(juros_pj$periodo, juros_pj$media, main="Juros PJ", type="l")
plot(basileia$periodo, basileia$media, main="Basileia", type="l")
plot(PIB$periodo, PIB$media, main="PIB", type="l")
plot(igp$periodo, igp$media, main="IGP", type="l")

# Para voltar ao padrão de 1 gráfico por tela depois:
par(mfrow = c(1, 1))


data_pf_ib <- data.frame(cbind((PIB),diff(igp), diff(inad_pf),diff(juros_pf),diff(basileia)))
colnames(data_pf_ib) <- c("pib", "igp", "inad_pf", "juros_pf", "basileia")
plot.ts(data_pf_ib)

data_pj_ib <- data.frame(cbind((PIB), diff(igp), diff(inad_pj),diff(juros_pj),diff(basileia)))
colnames(data_pj_ib) <- c("pib", "igp",  "inad_pj",  "juros_pj", "basileia")
plot.ts(data_pj_ib)

data_pf <- data.frame(cbind((PIB), diff(igp), diff(inad_pf),diff(juros_pf)))
colnames(data_pf) <- c("pib", "igp", "inad_pf", "juros_pf")
plot.ts(data_pf)

data_pj <- data.frame(cbind((PIB), diff(igp), diff(inad_pj),diff(juros_pj)))
colnames(data_pj) <- c("pib", "igp",  "inad_pj",  "juros_pj")
plot.ts(data_pj)

data_var <- data.frame(cbind((PIB), diff(igp), diff(inad_pj),diff(juros_pj),diff(basileia), diff(inad_pf),diff(juros_pf)))
colnames(data_var) <- c("pib", "igp",  "inad_pj",  "juros_pj", "basileia", "inad_pf", "juros_pf")
plot.ts(data_var)