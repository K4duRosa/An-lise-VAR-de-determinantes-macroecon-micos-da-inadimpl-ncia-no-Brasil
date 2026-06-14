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

VARselect(data_pj_ib, lag.max = 6, type = "both")

Var.Est_pj_f <- VAR(data_pj_ib, p=2, season = NULL, exogen = NULL)

# H0 não tem autocorrelação nos residuos
ACF <- serial.test(Var.Est_pj_f, lags.pt= 12, type = "PT.asymptotic")
ACF


plot.ts(residuals(Var.Est_pj_f))
acf(residuals(Var.Est_pj_f),36)
summary(Var.Est_pj_f)
roots(Var.Est_pj_f)
plot(ACF, names="pib")
plot(ACF, names="inad_pj")
plot(ACF, names="juros_pj")
plot(ACF, names="basileia")
plot(ACF, names="igp")

Box.test(juros_pj, 12, type="Ljung-Box")
ArchTest(juros_pj, lags=6, demean = TRUE)	

## Estimação do Modelo VAR:


normality.test(Var.Est_pj_f, multivariate.only = TRUE)





# Extrair resíduos do modelo atual
residuos <- residuals(Var.Est_pj_f)

# Padronizar e encontrar quem passa de 2.5 ou 3 desvios
outliers_info <- which(abs(scale(residuos)) > 2.5, arr.ind = TRUE)
print(outliers_info)

# 1. Lista de índices das linhas dos resíduos identificadas
linhas_residuos <- c(35,36,24)
#linhas_residuos <- c(35,36,24,41,34)

# 2. Ajustar para a linha correta nos dados originais (resíduo + p)
# Se seu VAR usa p=3:
indices_originais <- unique(linhas_residuos + 2)

# 3. Criar a matriz exógena
n_obs <- nrow(data_pj_ib) # use o dataframe que você entra no VAR
matriz_exog <- matrix(0, nrow = n_obs, ncol = length(indices_originais))

for(i in 1:length(indices_originais)) {
  matriz_exog[indices_originais[i], i] <- 1
}

colnames(matriz_exog) <- paste0("dummy_", indices_originais)



# Estimação corrigida
# 'type = const' é o padrão para dados em diferença
Var.Est_limpo_f <- VAR(data_pj_ib, p = 2, type = "const", exogen = matriz_exog)

# Verificando se a normalidade foi alcançada
teste_norm <- normality.test(Var.Est_limpo_f, multivariate.only = TRUE)
print(teste_norm)

plot.ts(residuals(Var.Est_limpo_f))
acf(residuals(Var.Est_limpo_f),36)
summary(Var.Est_limpo_f)
roots(Var.Est_limpo_f)
ACF <- serial.test(Var.Est_limpo_f, lags.pt= 12, type = "PT.asymptotic")
ACF

plot(ACF, names="pib")
plot(ACF, names="inad_pj") -49
plot(ACF, names="juros_pj") 5
plot(ACF, names="igp")

## Teste de Causalidade de Granger:

# 1. Definir a lista de variáveis atualizada
# Certifique-se de que esses nomes são exatamente iguais aos das colunas do seu modelo VAR
variaveis <- c("pib", "inad_pj", "igp", "juros_pj", "basileia")

# 2. Loop para gerar o "Block Exogeneity Wald Test" para cada variável
for (dep in variaveis) {
  
  cat("\n========================================================\n")
  cat("Dependent variable:", toupper(dep), "\n")
  cat("========================================================\n")
  cat(sprintf("%-15s %-12s %-12s\n", "Excluded", "Chi-sq", "Prob."))
  cat("--------------------------------------------------------\n")
  
  # Lista de variáveis independentes (todas menos a dependente atual)
  causas <- variaveis[variaveis != dep]
  
  # Loop para testes individuais (Linhas de cima de cada bloco)
  for (causa_unica in causas) {
    # No R, para testar a causalidade individual dentro do VAR:
    teste_ind <- causality(Var.Est_limpo_f, cause = causa_unica)
    
    # Extração dos resultados
    # Nota: statistic é o Qui-quadrado e p.value é a probabilidade
    chi_sq <- round(teste_ind$Granger$statistic, 4)
    prob <- round(teste_ind$Granger$p.value, 4)
    
    cat(sprintf("%-15s %-12s %-12s\n", causa_unica, chi_sq, prob))
  }
  
  # Teste Conjunto (Linha "ALL" do EViews)
  teste_all <- causality(Var.Est_limpo_f, cause = causas)
  
  cat("--------------------------------------------------------\n")
  cat(sprintf("%-15s %-12s %-12s\n", 
              "ALL", 
              round(teste_all$Granger$statistic, 4), 
              round(teste_all$Granger$p.value, 4)))
  cat("========================================================\n")
}

# -------------------------------------------------------------------------
# SCRIPT CONSOLIDADO: IRF ACUMULADA (ESTILO EVIEWS) - SEGMENTO PJ
# -------------------------------------------------------------------------

# 2. Gerar a Função Resposta a Impulso (IRF)
# Usando o modelo Pessoa Jurídica: Var.Est_limpo_f
irf_pj <- irf(
  Var.Est_limpo_f, 
  n.ahead = 10,           # Horizonte de 10 períodos
  ortho = FALSE,          # "Nonfactorized"
  cumulative = TRUE,      # "Accumulated Response"
  boot = TRUE,            
  ci = 0.95               # +/- 2 S.E.
)

# 3. Função de Extração de Dados (Corrigida para evitar erro de comprimento zero)
extract_irf_data <- function(irf_obj) {
  vars_impulse <- names(irf_obj$irf)
  vars_response <- colnames(irf_obj$irf[[1]])
  n_steps <- nrow(irf_obj$irf[[1]]) - 1
  
  df_list <- list()
  for(imp in vars_impulse) {
    for(res in vars_response) {
      temp_df <- data.frame(
        Step = 0:n_steps,
        Response = irf_obj$irf[[imp]][, res],
        Lower = irf_obj$Lower[[imp]][, res],
        Upper = irf_obj$Upper[[imp]][, res],
        Impulse = imp,
        ResponseVar = res
      )
      df_list[[paste(imp, res)]] <- temp_df
    }
  }
  return(bind_rows(df_list))
}

# 4. Processar os dados e organizar a ordem das variáveis
irf_results <- extract_irf_data(irf_pj)

# Definir a ordem exata das variáveis na grade (opcional, mas recomendado)
ordem_vars <- c("pib", "inad_pj", "igp", "juros_pj", "basileia")
irf_results$Impulse <- factor(irf_results$Impulse, levels = ordem_vars)
irf_results$ResponseVar <- factor(irf_results$ResponseVar, levels = ordem_vars)

# 5. Gerar a Grade de Gráficos (Matriz 5x5)
grafico_pj <- ggplot(irf_results, aes(x = Step, y = Response)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_line(aes(y = Lower), color = "red", linetype = "dashed", linewidth = 0.5) +
  geom_line(aes(y = Upper), color = "red", linetype = "dashed", linewidth = 0.5) +
  facet_grid(ResponseVar ~ Impulse, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Accumulated Response to Nonfactorized One S.D. Innovations ± 2 S.E.",
    subtitle = "Variáveis PJ: pib, inad_pj, igp, juros_pj, basileia",
    x = "Períodos",
    y = "Resposta Acumulada"
  ) +
  theme(
    strip.background = element_rect(fill = "gray95"),
    strip.text = element_text(size = 7, face = "bold"),
    axis.text = element_text(size = 6),
    panel.grid.minor = element_blank()
  )

# 6. Exibir e Salvar em arquivo grande para leitura legível
print(grafico_pj)
ggsave("irf_pj_completa.png", grafico_pj, width = 15, height = 12, dpi = 300)


