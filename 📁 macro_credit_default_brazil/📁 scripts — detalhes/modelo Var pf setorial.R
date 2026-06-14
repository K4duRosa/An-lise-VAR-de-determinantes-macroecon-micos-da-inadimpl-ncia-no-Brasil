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

PIB <- PIB[-1]
#igp <- igp[-1]

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

## Pré-Seleção do Modelo:


acf(data_pf_ib,36)
acf(data_pj_ib,36)
acf(data_pf,36)
acf(data_pj,36)



#######################################################################################
# 3. Escolha do Número de lag do VAR
#######################################################################################

VARselect(data_pf, lag.max = 6, type = "both")

Var.Est_pf_s <- VAR(data_pf, p=3, season = NULL, exogen = NULL)

# H0 não tem autocorrelação nos residuos
ACF <- serial.test(Var.Est_pf_s, lags.pt= 12, type = "PT.asymptotic")
ACF


plot.ts(residuals(Var.Est))
acf(residuals(Var.Est),36)
summary(Var.Est)
roots(Var.Est)


## Estimação do Modelo VAR:


normality.test(Var.Est, multivariate.only = TRUE)





# Extrair resíduos do modelo atual
residuos <- residuals(Var.Est_pf_s)

# Padronizar e encontrar quem passa de 2.5 ou 3 desvios
outliers_info <- which(abs(scale(residuos)) > 2.5, arr.ind = TRUE)
print(outliers_info)

# 1. Lista de índices das linhas dos resíduos identificadas
linhas_residuos <- c(34,35,46,32)
#c(34,35,46,32)
# 2. Ajustar para a linha correta nos dados originais (resíduo + p)
# Se seu VAR usa p=3:
indices_originais <- unique(linhas_residuos + 3)

# 3. Criar a matriz exógena
n_obs <- nrow(data_pf) # use o dataframe que você entra no VAR
matriz_exog <- matrix(0, nrow = n_obs, ncol = length(indices_originais))

for(i in 1:length(indices_originais)) {
  matriz_exog[indices_originais[i], i] <- 1
}

colnames(matriz_exog) <- paste0("dummy_", indices_originais)



# Estimação corrigida
# 'type = const' é o padrão para dados em diferença
Var.Est_limpo_pf_s <- VAR(data_pf, p = 3, type = "const", exogen = matriz_exog)

# Verificando se a normalidade foi alcançada
teste_norm <- normality.test(Var.Est_limpo_pf_s, multivariate.only = TRUE)
print(teste_norm)
summary(Var.Est_limpo)
ACF <- serial.test(Var.Est_limpo_pf_s, lags.pt= 12, type = "PT.asymptotic")
ACF
## Teste de Causalidade de Granger:

# Lista de todas as variáveis do seu modelo
variaveis <- c("pib", "inad_pf", "igp", "juros_pf")

# Criando um loop para analisar cada variável como 'Dependente'
for (dep in variaveis) {
  cat("\n==============================================\n")
  cat("Variável Dependente:", dep, "\n")
  cat("==============================================\n")
  
  # Lista de variáveis que podem causar a dependente (todas menos ela mesma)
  causas <- variaveis[variaveis != dep]
  
  # 1. Teste individual (Um por um, como as linhas da tabela do EViews)
  for (causa_unica in causas) {
    # Para testar individualmente no contexto do VAR, filtramos o efeito
    teste_ind <- causality(Var.Est_limpo_pf_s, cause = causa_unica)
    
    # Extraindo o P-valor e o Qui-quadrado (estatística de Wald)
    # Nota: No R, o teste foca na equação da 'outra' variável, 
    # por isso o output padrão pode parecer invertido.
    cat(paste0("Excluído: ", causa_unica), 
        "| Chi-sq:", round(teste_ind$Granger$statistic, 4), 
        "| Prob:", round(teste_ind$Granger$p.value, 4), "\n")
  }
  
  # 2. Teste Conjunto (A linha 'ALL' da sua imagem)
  teste_all <- causality(Var.Est_limpo_pf_s, cause = causas)
  cat("----------------------------------------------\n")
  cat("TODAS (ALL)   | Chi-sq:", round(teste_all$Granger$statistic, 4), 
      "| Prob:", round(teste_all$Granger$p.value, 4), "\n\n")
}##

###################################################################

# 1. Gerar IRF
irf_pf_obj <- irf(Var.Est_limpo_pf_s, n.ahead = 10, ortho = FALSE, 
                  cumulative = TRUE, boot = TRUE, ci = 0.95)
#2. Extrair dados ignorando possíveis colunas fantasmas
extract_irf_clean <- function(obj) {
  df_list <- list()
  # Usa nomes extraídos DIRETAMENTE do objeto para evitar NAs
  imp_names <- names(obj$irf)
  res_names <- colnames(obj$irf[[1]])
  
  for(imp in imp_names) {
    for(res in res_names) {
      df_list[[paste(imp, res)]] <- data.frame(
        Step = 0:(nrow(obj$irf[[1]]) - 1),
        Response = obj$irf[[imp]][, res],
        Lower = obj$Lower[[imp]][, res],
        Upper = obj$Upper[[imp]][, res],
        Impulse = imp, 
        ResponseVar = res
      )
    }
  }
  return(dplyr::bind_rows(df_list))
}


# 2. Extrair dados (Reutiliza a função clean)
data_pf_plot <- extract_irf_clean(irf_pf_obj)

# 3. Plotar
ggplot(data_pf_plot, aes(x = Step, y = Response)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_line(aes(y = Lower), color = "red", linetype = "dashed") +
  geom_line(aes(y = Upper), color = "red", linetype = "dashed") +
  facet_grid(ResponseVar ~ Impulse, scales = "free_y") +
  theme_bw() +
  labs(title = "PF: Accumulated Response (Nonfactorized)")

