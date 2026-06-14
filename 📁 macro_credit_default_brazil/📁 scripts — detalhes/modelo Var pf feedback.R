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

VARselect(data_pf_ib, lag.max = 6, type = "both")

Var.Est_pf_f <- VAR(data_pf_ib, p=1, season = NULL, exogen = NULL)

# H0 não tem autocorrelação nos residuos
ACF <- serial.test(Var.Est_pf_f, lags.pt= 12, type = "PT.asymptotic")
ACF


plot.ts(residuals(Var.Est_pf_f))
acf(residuals(Var.Est_pf_f),36)
summary(Var.Est_pf_f)
roots(Var.Est_pf_f)
plot(ACF, names="pib")
plot(ACF, names="inad_pf")
plot(ACF, names="juros_pf")
plot(ACF, names="basileia")
plot(ACF, names="igp")



## Estimação do Modelo VAR:


normality.test(Var.Est_pf_f, multivariate.only = TRUE)





# Extrair resíduos do modelo atual
residuos <- residuals(Var.Est_pf_f)

# Padronizar e encontrar quem passa de 2.5 ou 3 desvios
outliers_info <- which(abs(scale(residuos)) > 2.5, arr.ind = TRUE)
print(outliers_info)

# 1. Lista de índices das linhas dos resíduos identificadas
linhas_residuos <- c(36,37,49)
  #c(35,36,49,24,41) e 49
#c(11,35,22,25,42) melhor até agora
# 2. Ajustar para a linha correta nos dados originais (resíduo + p)
# Se seu VAR usa p=3:
indices_originais <- unique(linhas_residuos + 1)

# 3. Criar a matriz exógena
n_obs <- nrow(data_pj_ib) # use o dataframe que você entra no VAR
matriz_exog <- matrix(0, nrow = n_obs, ncol = length(indices_originais))

for(i in 1:length(indices_originais)) {
  matriz_exog[indices_originais[i], i] <- 1
}

colnames(matriz_exog) <- paste0("dummy_", indices_originais)



# Estimação corrigida
# 'type = const' é o padrão para dados em diferença
Var.Est_limpo_pf_f <- VAR(data_pf_ib, p = 1, type = "const", exogen = matriz_exog)

# Verificando se a normalidade foi alcançada
teste_norm <- normality.test(Var.Est_limpo_pf_f, multivariate.only = TRUE)
print(teste_norm)

plot.ts(residuals(Var.Est_limpo))
acf(residuals(Var.Est_limpo),36)
summary(Var.Est_limpo)
roots(Var.Est_limpo)
ACF <- serial.test(Var.Est_limpo_pf_f, lags.pt= 12, type = "PT.asymptotic")
ACF

plot(ACF, names="pib")
plot(ACF, names="inad_pf") -49
plot(ACF, names="juros_pf") 5
plot(ACF, names="igp")

## Teste de Causalidade de Granger:

# 1. Definir a lista de variáveis atualizada
# Certifique-se de que esses nomes são exatamente iguais aos das colunas do seu modelo VAR
variaveis <- c("pib", "inad_pf", "igp", "juros_pf", "basileia")

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
    teste_ind <- causality(Var.Est_limpo_pf_f, cause = causa_unica)
    
    # Extração dos resultados
    # Nota: statistic é o Qui-quadrado e p.value é a probabilidade
    chi_sq <- round(teste_ind$Granger$statistic, 4)
    prob <- round(teste_ind$Granger$p.value, 4)
    
    cat(sprintf("%-15s %-12s %-12s\n", causa_unica, chi_sq, prob))
  }
  
  # Teste Conjunto (Linha "ALL" do EViews)
  teste_all <- causality(Var.Est_limpo_pf_f, cause = causas)
  
  cat("--------------------------------------------------------\n")
  cat(sprintf("%-15s %-12s %-12s\n", 
              "ALL", 
              round(teste_all$Granger$statistic, 4), 
              round(teste_all$Granger$p.value, 4)))
  cat("========================================================\n")
}


# Gerando a IRF com as configurações exatas da imagem:
irf_pf <- irf(
  Var.Est_limpo_pf_f, 
  n.ahead = 10,           # Horizonte de 10 períodos
  ortho = FALSE,          # "Nonfactorized" (Choques não ortogonalizados)
  cumulative = TRUE,      # "Accumulated Response"
  boot = TRUE,            # Necessário para as bandas de erro
  ci = 0.95               # Equivale a aproximadamente ± 2 S.E.
)

# Para visualizar
plot(irf_pf)

#========================================================
# Função para transformar o objeto IRF em um Data Frame
extract_irf_corrigida <- function(irf_obj) {
  vars_impulse <- names(irf_obj$irf)
  vars_response <- colnames(irf_obj$irf[[1]])
  
  # Detecta o número de passos (steps) dinamicamente
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
  return(do.call(rbind, df_list))
}

# Aplicando a função corrigida
irf_data <- extract_irf_corrigida(irf_pf)

library(ggplot2)

# Criando o gráfico em grade
p <- ggplot(irf_data, aes(x = Step, y = Response)) +
  # Linha de referência no zero
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  # Linha da Resposta (Azul)
  geom_line(color = "blue", size = 0.7) +
  # Bandas de Erro (Vermelho Tracejado - 2 S.E.)
  geom_line(aes(y = Lower), color = "red", linetype = "dashed") +
  geom_line(aes(y = Upper), color = "red", linetype = "dashed") +
  # Organização em Grade: Linhas = Respostas, Colunas = Impulsos
  facet_grid(ResponseVar ~ Impulse, scales = "free_y") + 
  theme_bw() + 
  labs(
    title = "Accumulated Response to Nonfactorized One S.D. Innovations ± 2 S.E.",
    subtitle = "Variáveis: PIB, INAD_PF, IGP, JUROS_PF, BASILEIA",
    x = "Períodos (Steps)",
    y = "Resposta Acumulada"
  ) +
  theme(
    strip.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(size = 7),
    panel.grid.minor = element_blank()
  )

# Visualizar
print(p)
