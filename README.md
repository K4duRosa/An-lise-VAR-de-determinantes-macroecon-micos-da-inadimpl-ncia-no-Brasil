# Determinantes Macroeconômicos da Inadimplência no Brasil

Projeto acadêmico desenvolvido como parte da graduação em Economia na UFABC.

## Objetivo
Modelar os determinantes macroeconômicos da inadimplência de pessoas físicas e jurídicas
no Brasil utilizando modelos VAR (Vetores Autorregressivos).

## Variáveis
- PIB (crescimento real)
- IGP-M (inflação)
- Taxa de juros (Selic)
- Índice de Basileia (capitalização bancária)
- Inadimplência (PF e PJ)

## Metodologia
- Teste de raiz unitária (ADF, KPSS e PP)
- Modelos VAR com e sem Índice de Basileia
- Causalidade de Granger
- Funções de Impulso-Resposta (IRF)

## Estrutura
- `scripts/` — scripts R numerados por ordem de execução
- `data/` — séries temporais utilizadas
- `outputs/` — gráficos e tabelas gerados

## Fonte dos dados
Banco Central do Brasil (Inadimplência,índice de Basileia e Juros), Fed(IBGE) e FGV(IGP-M).

## Linguagem
R — pacotes principais: 
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
