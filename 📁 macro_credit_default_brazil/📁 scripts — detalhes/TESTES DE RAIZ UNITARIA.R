# 1. Instalar e carregar pacotes necessários


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



setwd("C:/Temp/AST")
igp <-read.csv2("igp-m-fgv.csv", header = TRUE)
igp <- na.omit(as.numeric(igp$Índice))

PIB <- read.csv2("C:/Temp/AST/pib br fed (2).csv", header = TRUE)
PIB <- na.omit(as.numeric(PIB$valor))

# 2. Coletar dados do Banco Central



# Função auxiliar para processar média trimestral
processar_trimestre <- function(id, nome, data_inicio) {
  df <- gbcbd_get_series(id, first.date = data_inicio, last.date = Sys.Date())
  
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
data_ref <- "2011-03-01"

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

length(PIB[-1])
length(igp)
length(inad_pj)
length(juros_pj)



##################################################################################################
# Etapa 1: Testar de Raiz unitaria
##################################################################################################

#teste DF

# IGP - Drift
df.c.igp_d0 <- ur.df(igp, type="drift", lags=0)
summary(df.c.igp_d0)
# não rejeita H0  p-value: 0.7498

df.c.igp_d1 <- ur.df(diff(igp), type="drift", lags=0)
summary(df.c.igp_d1)
#Rejeita H0 a 5%  p-value:  0.002543

# IGP - Trend
df.c.igp_t0 <- ur.df(igp, type="trend", lags=0)
summary(df.c.igp_t0)
#não rejeita p-value:  0.2724
df.c.igp_t1 <- ur.df(diff(igp), type="trend", lags=0)
summary(df.c.igp_t1)
#−3.314 < −3.15 → rejeita H₀ a 10% −3.314 > −3.45 → não rejeita a 5 %p-value: 0.006116


# INAD_PF - Drift
df.c.inad_pf_d0 <- ur.df(inad_pf, type="drift", lags=0)
summary(df.c.inad_pf_d0)
df.c.inad_pf_d1 <- ur.df(diff(inad_pf), type="drift", lags=0)
summary(df.c.inad_pf_d1)

# INAD_PF - Trend
df.c.inad_pf_t0 <- ur.df(inad_pf, type="trend", lags=0)
summary(df.c.inad_pf_t0)
df.c.inad_pf_t1 <- ur.df(diff(inad_pf), type="trend", lags=0)
summary(df.c.inad_pf_t1)


# INAD_PJ - Drift
df.c.inad_pj_d0 <- ur.df(inad_pj, type="drift", lags=0)
summary(df.c.inad_pj_d0)
df.c.inad_pj_d1 <- ur.df(diff(inad_pj), type="drift", lags=0)
summary(df.c.inad_pj_d1)

# INAD_PJ - Trend
df.c.inad_pj_t0 <- ur.df(inad_pj, type="trend", lags=0)
summary(df.c.inad_pj_t0)
df.c.inad_pj_t1 <- ur.df(diff(inad_pj), type="trend", lags=0)
summary(df.c.inad_pj_t1)

# JUROS_PF - Drift
df.c.juros_pf_d0 <- ur.df(juros_pf, type="drift", lags=0)
summary(df.c.juros_pf_d0)
df.c.juros_pf_d1 <- ur.df(diff(juros_pf), type="drift", lags=0)
summary(df.c.juros_pf_d1)

# JUROS_PF - Trend
df.c.juros_pf_t0 <- ur.df(juros_pf, type="trend", lags=0)
summary(df.c.juros_pf_t0)
df.c.juros_pf_t1 <- ur.df(diff(juros_pf), type="trend", lags=0)
summary(df.c.juros_pf_t1)


# JUROS_PJ - Drift
df.c.juros_pj_d0 <- ur.df(juros_pj, type="drift", lags=0)
summary(df.c.juros_pj_d0)
df.c.juros_pj_d1 <- ur.df(diff(juros_pj), type="drift", lags=0)
summary(df.c.juros_pj_d1)

# JUROS_PJ - Trend
df.c.juros_pj_t0 <- ur.df(juros_pj, type="trend", lags=0)
summary(df.c.juros_pj_t0)
df.c.juros_pj_t1 <- ur.df(diff(juros_pj), type="trend", lags=0)
summary(df.c.juros_pj_t1)

# PIB - Drift
df.c.pib_d0 <- ur.df(PIB, type="drift", lags=0)
summary(df.c.pib_d0)
df.c.pib_d1 <- ur.df(diff(PIB), type="drift", lags=0)
summary(df.c.pib_d1)

# PIB - Trend
df.c.pib_t0 <- ur.df(PIB, type="trend", lags=0)
summary(df.c.pib_t0)
df.c.pib_t1 <- ur.df(diff(PIB), type="trend", lags=0)
summary(df.c.pib_t1)


# BASILEIA - Drift
df.c.basileia_d0 <- ur.df(basileia, type="drift", lags=0)
summary(df.c.basileia_d0)
df.c.basileia_d1 <- ur.df(diff(basileia), type="drift", lags=0)
summary(df.c.basileia_d1)

# BASILEIA - Trend
df.c.basileia_t0 <- ur.df(basileia, type="trend", lags=0)
summary(df.c.basileia_t0)
df.c.basileia_t1 <- ur.df(diff(basileia), type="trend", lags=0)
summary(df.c.basileia_t1)

#teste PP

pp.c.igp_d0 <- ur.pp(igp,
                     type  = "Z-tau",
                     model = "constant",
                     lags  = "short")
summary(pp.c.igp_d0)

pp.c.igp_d1 <- ur.pp(diff(igp),
                     type  = "Z-tau",
                     model = "constant",
                     lags  = "short")
summary(pp.c.igp_d1)

pp.c.igp_t0 <- ur.pp(igp,
                     type  = "Z-tau",
                     model = "trend",
                     lags  = "short")
summary(pp.c.igp_t0)

pp.c.igp_t1 <- ur.pp(diff(igp),
                     type  = "Z-tau",
                     model = "trend",
                     lags  = "short")
summary(pp.c.igp_t1)


pp.c.inad_pf_d0 <- ur.pp(inad_pf,
                         type  = "Z-tau",
                         model = "constant",
                         lags  = "short")
summary(pp.c.inad_pf_d0)

pp.c.inad_pf_d1 <- ur.pp(diff(inad_pf),
                         type  = "Z-tau",
                         model = "constant",
                         lags  = "short")
summary(pp.c.inad_pf_d1)

pp.c.inad_pf_t0 <- ur.pp(inad_pf,
                         type  = "Z-tau",
                         model = "trend",
                         lags  = "short")
summary(pp.c.inad_pf_t0)

pp.c.inad_pf_t1 <- ur.pp(diff(inad_pf),
                         type  = "Z-tau",
                         model = "trend",
                         lags  = "short")
summary(pp.c.inad_pf_t1)

pp.c.inad_pj_d0 <- ur.pp(inad_pj,
                         type  = "Z-tau",
                         model = "constant",
                         lags  = "short")
summary(pp.c.inad_pj_d0)

pp.c.inad_pj_d1 <- ur.pp(diff(inad_pj),
                         type  = "Z-tau",
                         model = "constant",
                         lags  = "short")
summary(pp.c.inad_pj_d1)

pp.c.inad_pj_t0 <- ur.pp(inad_pj,
                         type  = "Z-tau",
                         model = "trend",
                         lags  = "short")
summary(pp.c.inad_pj_t0)

pp.c.inad_pj_t1 <- ur.pp(diff(inad_pj),
                         type  = "Z-tau",
                         model = "trend",
                         lags  = "short")
summary(pp.c.inad_pj_t1)

pp.c.juros_pf_d0 <- ur.pp(juros_pf,
                          type  = "Z-tau",
                          model = "constant",
                          lags  = "short")
summary(pp.c.juros_pf_d0)

pp.c.juros_pf_d1 <- ur.pp(diff(juros_pf),
                          type  = "Z-tau",
                          model = "constant",
                          lags  = "short")
summary(pp.c.juros_pf_d1)

pp.c.juros_pf_t0 <- ur.pp(juros_pf,
                          type  = "Z-tau",
                          model = "trend",
                          lags  = "short")
summary(pp.c.juros_pf_t0)

pp.c.juros_pf_t1 <- ur.pp(diff(juros_pf),
                          type  = "Z-tau",
                          model = "trend",
                          lags  = "short")
summary(pp.c.juros_pf_t1)

pp.c.juros_pj_d0 <- ur.pp(juros_pj,
                          type  = "Z-tau",
                          model = "constant",
                          lags  = "short")
summary(pp.c.juros_pj_d0)

pp.c.juros_pj_d1 <- ur.pp(diff(juros_pj),
                          type  = "Z-tau",
                          model = "constant",
                          lags  = "short")
summary(pp.c.juros_pj_d1)

pp.c.juros_pj_t0 <- ur.pp(juros_pj,
                          type  = "Z-tau",
                          model = "trend",
                          lags  = "short")
summary(pp.c.juros_pj_t0)

pp.c.juros_pj_t1 <- ur.pp(diff(juros_pj),
                          type  = "Z-tau",
                          model = "trend",
                          lags  = "short")
summary(pp.c.juros_pj_t1)

pp.c.pib_d0 <- ur.pp(PIB,
                     type  = "Z-tau",
                     model = "constant",
                     lags  = "short")
summary(pp.c.pib_d0)

pp.c.pib_d1 <- ur.pp(diff(PIB),
                     type  = "Z-tau",
                     model = "constant",
                     lags  = "short")
summary(pp.c.pib_d1)

pp.c.pib_t0 <- ur.pp(PIB,
                     type  = "Z-tau",
                     model = "trend",
                     lags  = "short")
summary(pp.c.pib_t0)

pp.c.pib_t1 <- ur.pp(diff(PIB),
                     type  = "Z-tau",
                     model = "trend",
                     lags  = "short")
summary(pp.c.pib_t1)

pp.c.basileia_d0 <- ur.pp(basileia,
                          type  = "Z-tau",
                          model = "constant",
                          lags  = "short")
summary(pp.c.basileia_d0)

pp.c.basileia_d1 <- ur.pp(diff(basileia),
                          type  = "Z-tau",
                          model = "constant",
                          lags  = "short")
summary(pp.c.basileia_d1)

pp.c.basileia_t0 <- ur.pp(basileia,
                          type  = "Z-tau",
                          model = "trend",
                          lags  = "short")
summary(pp.c.basileia_t0)

pp.c.basileia_t1 <- ur.pp(diff(basileia),
                          type  = "Z-tau",
                          model = "trend",
                          lags  = "short")
summary(pp.c.basileia_t1)


#Teste KPSS

library(urca)

# =========================
# IGP
# =========================
kpss.c.igp_mu0 <- ur.kpss(igp, type = "mu", lags = "short")
summary(kpss.c.igp_mu0)

kpss.c.igp_mu1 <- ur.kpss(diff(igp), type = "mu", lags = "short")
summary(kpss.c.igp_mu1)

kpss.c.igp_t0 <- ur.kpss(igp, type = "tau", lags = "short")
summary(kpss.c.igp_t0)

kpss.c.igp_t1 <- ur.kpss(diff(igp), type = "tau", lags = "short")
summary(kpss.c.igp_t1)


# =========================
# INAD_PF
# =========================
kpss.c.inad_pf_mu0 <- ur.kpss(inad_pf, type = "mu", lags = "short")
summary(kpss.c.inad_pf_mu0)

kpss.c.inad_pf_mu1 <- ur.kpss(diff(inad_pf), type = "mu", lags = "short")
summary(kpss.c.inad_pf_mu1)

kpss.c.inad_pf_t0 <- ur.kpss(inad_pf, type = "tau", lags = "short")
summary(kpss.c.inad_pf_t0)

kpss.c.inad_pf_t1 <- ur.kpss(diff(inad_pf), type = "tau", lags = "short")
summary(kpss.c.inad_pf_t1)


# =========================
# INAD_PJ
# =========================
kpss.c.inad_pj_mu0 <- ur.kpss(inad_pj, type = "mu", lags = "short")
summary(kpss.c.inad_pj_mu0)

kpss.c.inad_pj_mu1 <- ur.kpss(diff(inad_pj), type = "mu", lags = "short")
summary(kpss.c.inad_pj_mu1)

kpss.c.inad_pj_t0 <- ur.kpss(inad_pj, type = "tau", lags = "short")
summary(kpss.c.inad_pj_t0)

kpss.c.inad_pj_t1 <- ur.kpss(diff(inad_pj), type = "tau", lags = "short")
summary(kpss.c.inad_pj_t1)


# =========================
# JUROS_PF
# =========================
kpss.c.juros_pf_mu0 <- ur.kpss(juros_pf, type = "mu", lags = "short")
summary(kpss.c.juros_pf_mu0)

kpss.c.juros_pf_mu1 <- ur.kpss(diff(juros_pf), type = "mu", lags = "short")
summary(kpss.c.juros_pf_mu1)

kpss.c.juros_pf_t0 <- ur.kpss(juros_pf, type = "tau", lags = "short")
summary(kpss.c.juros_pf_t0)

kpss.c.juros_pf_t1 <- ur.kpss(diff(juros_pf), type = "tau", lags = "short")
summary(kpss.c.juros_pf_t1)


# =========================
# JUROS_PJ
# =========================
kpss.c.juros_pj_mu0 <- ur.kpss(juros_pj, type = "mu", lags = "short")
summary(kpss.c.juros_pj_mu0)

kpss.c.juros_pj_mu1 <- ur.kpss(diff(juros_pj), type = "mu", lags = "short")
summary(kpss.c.juros_pj_mu1)

kpss.c.juros_pj_t0 <- ur.kpss(juros_pj, type = "tau", lags = "short")
summary(kpss.c.juros_pj_t0)

kpss.c.juros_pj_t1 <- ur.kpss(diff(juros_pj), type = "tau", lags = "short")
summary(kpss.c.juros_pj_t1)


# =========================
# PIB
# =========================
kpss.c.pib_mu0 <- ur.kpss(PIB, type = "mu", lags = "short")
summary(kpss.c.pib_mu0)

kpss.c.pib_mu1 <- ur.kpss(diff(PIB), type = "mu", lags = "short")
summary(kpss.c.pib_mu1)

kpss.c.pib_t0 <- ur.kpss(PIB, type = "tau", lags = "short")
summary(kpss.c.pib_t0)

kpss.c.pib_t1 <- ur.kpss(diff(PIB), type = "tau", lags = "short")
summary(kpss.c.pib_t1)


# =========================
# BASILEIA
# =========================
kpss.c.basileia_mu0 <- ur.kpss(basileia, type = "mu", lags = "short")
summary(kpss.c.basileia_mu0)

kpss.c.basileia_mu1 <- ur.kpss(diff(basileia), type = "mu", lags = "short")
summary(kpss.c.basileia_mu1)

kpss.c.basileia_t0 <- ur.kpss(basileia, type = "tau", lags = "short")
summary(kpss.c.basileia_t0)

kpss.c.basileia_t1 <- ur.kpss(diff(basileia), type = "tau", lags = "short")
summary(kpss.c.basileia_t1)
